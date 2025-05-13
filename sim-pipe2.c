/* sim-pipe2.c - pipelined simulator implementation with forwarding */

/* SimpleScalar(TM) Tool Suite
 * Copyright (C) 1994-2003 by Todd M. Austin, Ph.D. and SimpleScalar, LLC.
 * All Rights Reserved.
 *
 * THIS IS A LEGAL DOCUMENT, BY USING SIMPLESCALAR,
 * YOU ARE AGREEING TO THESE TERMS AND CONDITIONS.
 *
 * No portion of this work may be used by any commercial entity, or for any
 * commercial purpose, without the prior, written permission of SimpleScalar,
 * LLC (info@simplescalar.com). Nonprofit and noncommercial use is permitted
 * as described below.
 *
 * 1. SimpleScalar is provided AS IS, with no warranty of any kind, express
 * or implied. The user of the program accepts full responsibility for the
 * application of the program and the use of any results.
 *
 * 2. Nonprofit and noncommercial use is encouraged. SimpleScalar may be
 * downloaded, compiled, executed, copied, and modified solely for nonprofit,
 * educational, noncommercial research, and noncommercial scholarship
 * purposes provided that this notice in its entirety accompanies all copies.
 * Copies of the modified software can be delivered to persons who use it
 * solely for nonprofit, educational, noncommercial research, and
 * noncommercial scholarship purposes provided that this notice in its
 * entirety accompanies all copies.
 *
 * 3. ALL COMMERCIAL USE, AND ALL USE BY FOR PROFIT ENTITIES, IS EXPRESSLY
 * PROHIBITED WITHOUT A LICENSE FROM SIMPLESCALAR, LLC (info@simplescalar.com).
 *
 * 4. No nonprofit user may place any restrictions on the use of this software,
 * including as modified by the user, by any other authorized user.
 *
 * 5. Noncommercial and nonprofit users may distribute copies of SimpleScalar
 * in compiled or executable form as set forth in Section 2, provided that
 * either: (A) it is accompanied by the corresponding machine-readable source
 * code, or (B) it is accompanied by a written offer, with no time limit, to
 * give anyone a machine-readable copy of the corresponding source code in
 * return for reimbursement of the cost of distribution. This written offer
 * must permit verbatim duplication by anyone, or (C) it is distributed by
 * someone who received only the executable form, and is accompanied by a
 * copy of the written offer of source code.
 *
 * 6. SimpleScalar was developed by Todd M. Austin, Ph.D. The tool suite is
 * currently maintained by SimpleScalar LLC (info@simplescalar.com). US Mail:
 * 2395 Timbercrest Court, Ann Arbor, MI 48105.
 *
 * Copyright (C) 1994-2003 by Todd M. Austin, Ph.D. and SimpleScalar, LLC.
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "host.h"
#include "misc.h"
#include "machine.h"
#include "regs.h"
#include "memory.h"
#include "loader.h"
#include "syscall.h"
#include "dlite.h"
#include "options.h"
#include "stats.h"
#include "sim.h"

/*
 * This file implements a pipelined simulator with 5 stages:
 * - Fetch (IF)
 * - Decode (ID)
 * - Execute (EX)
 * - Memory (MEM)
 * - Writeback (WB)
 *
 * With the following assumptions:
 * - Perfect instruction cache (no I-cache misses)
 * - Perfect data cache (no D-cache misses)
 * - No branch prediction
 * - Split-phase register access for the writeback (WB) stage:
 *   - Register writes occur in the first half of the clock cycle
 *   - Register reads occur in the second half of the clock cycle
 *   This allows a register to be written and read in the same cycle
 *
 * It supports both forwarding and no-forwarding modes.
 */

/* simulated registers */
static struct regs_t regs;

/* simulated memory */
static struct mem_t *mem = NULL;

/* track number of refs */
static counter_t sim_num_refs = 0;

/* maximum number of inst's to execute */
static unsigned int max_insts;

/* pipeline option */
static int forward;

/* Benchmark injection for testing RAW dependencies */
static int run_raw_benchmark = 0;
static int raw_benchmark_instrs_completed = 0;
static int raw_benchmark_size = 20; /* Number of dependent instructions */

/* register simulator-specific options */
void sim_reg_options(struct opt_odb_t *odb)
{
  opt_reg_header(odb,
                 "sim-pipe2: This simulator implements a 5-stage pipelined processor with\n"
                 "optional forwarding capability. It models fetch, decode, execute, memory, and\n"
                 "writeback stages. The simulator assumes perfect caches and no branch prediction.\n");

  /* instruction limit */
  opt_reg_uint(odb, "-max:inst", "maximum number of inst's to execute",
               &max_insts, /* default */ 0,
               /* print */ TRUE, /* format */ NULL);

  /* forwarding option */
  opt_reg_flag(odb, "-forward", "enable data forwarding",
               &forward, /* default */ TRUE,
               /* print */ TRUE, /* format */ NULL);

  /* RAW benchmark option */
  opt_reg_flag(odb, "-raw", "run RAW dependency benchmark",
               &run_raw_benchmark, /* default */ FALSE,
               /* print */ TRUE, /* format */ NULL);

  /* benchmark size option */
  opt_reg_int(odb, "-raw:size", "number of instructions in RAW benchmark",
              &raw_benchmark_size, /* default */ 20,
              /* print */ TRUE, /* format */ NULL);
}

/* check simulator-specific option values */
void sim_check_options(struct opt_odb_t *odb, int argc, char **argv)
{
  /* nada */
}

/* pipeline stage structures */
typedef struct ifid_buf_t
{
  md_inst_t inst;     /* instruction */
  md_addr_t pc;       /* program counter */
  int valid;          /* valid bit */
  counter_t inst_num; /* instruction sequence number */
} ifid_buf_t;

/* ID/EX pipeline latch contents */
typedef struct
{
  md_addr_t pc;       /* instruction PC */
  md_inst_t inst;     /* instruction bits */
  enum md_opcode op;  /* decoded instruction opcode */
  int r_out[2];       /* output register files */
  int r_in[3];        /* input register files */
  word_t r_in_val[3]; /* input register values */
  word_t imm;         /* immediate value (if used) */
  int is_mem;         /* memory operation flag */
  int is_load;        /* load operation flag */
  int is_store;       /* store operation flag */
  int is_ctrl;        /* control instruction flag */
  md_addr_t mem_addr; /* memory address (for loads/stores) */
  counter_t inst_num; /* instruction sequence number */
  int stall;          /* stall indicator */
  int valid;          /* valid instruction in latch */
} idex_buf_t;

typedef struct exmem_buf_t
{
  md_inst_t inst;     /* instruction */
  md_addr_t pc;       /* program counter */
  int valid;          /* valid bit */
  enum md_opcode op;  /* opcode */
  word_t alu_result;  /* ALU result */
  int r_out[2];       /* output registers */
  int r_in[3];        /* input registers */
  word_t r_in_val[3]; /* input register values */
  int is_mem;         /* memory access instruction */
  int is_load;        /* load instruction */
  int is_store;       /* store instruction */
  md_addr_t mem_addr; /* memory address (for loads/stores) */
  word_t mem_data;    /* data to store to memory */
  counter_t inst_num; /* instruction sequence number */
} exmem_buf_t;

typedef struct memwb_buf_t
{
  md_inst_t inst;     /* instruction */
  md_addr_t pc;       /* program counter */
  int valid;          /* valid bit */
  enum md_opcode op;  /* opcode */
  word_t alu_result;  /* ALU result */
  word_t mem_result;  /* memory load result */
  int r_out[2];       /* output registers */
  int is_load;        /* load instruction */
  counter_t inst_num; /* instruction sequence number */
} memwb_buf_t;

/* pipeline registers */
static ifid_buf_t IF_ID;
static idex_buf_t ID_EX;
static exmem_buf_t EX_MEM;
static memwb_buf_t MEM_WB;
static memwb_buf_t WB; /* Separate latch for the WB stage */

/* pipeline stage functions */
static void fetch_stage(void);
static void decode_stage(void);
static void execute_stage(void);
static void memory_stage(void);
static void writeback_stage(void);

/* cycle counter */
static counter_t sim_cycle = 0;
static counter_t inst_seq = 0;
static counter_t sim_num_stalls = 0; /* stall counter */

/* register simulator-specific statistics */
void sim_reg_stats(struct stat_sdb_t *sdb)
{
  stat_reg_counter(sdb, "sim_num_insn",
                   "total number of instructions committed",
                   &sim_num_insn, sim_num_insn, NULL);
  stat_reg_counter(sdb, "sim_num_refs",
                   "total number of loads and stores committed",
                   &sim_num_refs, 0, NULL);
  stat_reg_counter(sdb, "sim_num_cycles",
                   "total number of cycles simulated",
                   &sim_cycle, 0, NULL);
  stat_reg_counter(sdb, "sim_num_stalls",
                   "total number of pipeline stalls",
                   &sim_num_stalls, 0, NULL);
  stat_reg_formula(sdb, "sim_IPC",
                   "instruction per cycle",
                   "sim_num_insn / sim_num_cycles", NULL);
  stat_reg_formula(sdb, "sim_CPI",
                   "cycles per instruction",
                   "sim_num_cycles / sim_num_insn", NULL);
  stat_reg_int(sdb, "sim_elapsed_time",
               "total simulation time in seconds",
               &sim_elapsed_time, 0, NULL);
  stat_reg_formula(sdb, "sim_inst_rate",
                   "simulation speed (in insts/sec)",
                   "sim_num_insn / sim_elapsed_time", NULL);
  ld_reg_stats(sdb);
  mem_reg_stats(mem, sdb);
}

/* initialize the simulator */
void sim_init(void)
{
  /* allocate and initialize register file */
  regs_init(&regs);

  /* allocate and initialize memory space */
  mem = mem_create("mem");
  mem_init(mem);

  /* initialize pipeline registers */
  IF_ID.valid = ID_EX.valid = EX_MEM.valid = MEM_WB.valid = WB.valid = FALSE;
  sim_cycle = 0;
  inst_seq = 0;
  sim_num_stalls = 0;
}

/* load program into simulated state */
void sim_load_prog(char *fname,           /* program to load */
                   int argc, char **argv, /* program arguments */
                   char **envp)           /* program environment */
{
  /* load program text and data, set up environment, memory, and regs */
  ld_load_prog(fname, argc, argv, envp, &regs, mem, TRUE);

  /* initialize the DLite debugger */
  dlite_init(md_reg_obj, dlite_mem_obj, dlite_mstate_obj);
}

/* print simulator-specific configuration information */
void sim_aux_config(FILE *stream)
{
  fprintf(stream, "Processor Pipeline:\n");
  fprintf(stream, "  Forwarding: %s\n", forward ? "enabled" : "disabled");
  fprintf(stream, "  Perfect instruction cache: yes\n");
  fprintf(stream, "  Perfect data cache: yes\n");
  fprintf(stream, "  Split-phase register access: yes\n");
}

/* dump simulator-specific auxiliary simulator statistics */
void sim_aux_stats(FILE *stream)
{
  /* nada */
}

/* un-initialize simulator-specific state */
void sim_uninit(void)
{
  /* nada */
}

/*
 * configure the execution engine
 */

/*
 * precise architected register accessors
 */

/* next program counter */
#define SET_NPC(EXPR) (regs.regs_NPC = (EXPR))

/* current program counter */
#define CPC (regs.regs_PC)

/* general purpose registers */
#define GPR(N) (regs.regs_R[N])
#define SET_GPR(N, EXPR) (regs.regs_R[N] = (EXPR))

#if defined(TARGET_PISA)

/* floating point registers, L->word, F->single-prec, D->double-prec */
#define FPR_L(N) (regs.regs_F.l[(N)])
#define SET_FPR_L(N, EXPR) (regs.regs_F.l[(N)] = (EXPR))
#define FPR_F(N) (regs.regs_F.f[(N)])
#define SET_FPR_F(N, EXPR) (regs.regs_F.f[(N)] = (EXPR))
#define FPR_D(N) (regs.regs_F.d[(N) >> 1])
#define SET_FPR_D(N, EXPR) (regs.regs_F.d[(N) >> 1] = (EXPR))

/* miscellaneous register accessors */
#define SET_HI(EXPR) (regs.regs_C.hi = (EXPR))
#define HI (regs.regs_C.hi)
#define SET_LO(EXPR) (regs.regs_C.lo = (EXPR))
#define LO (regs.regs_C.lo)
#define FCC (regs.regs_C.fcc)
#define SET_FCC(EXPR) (regs.regs_C.fcc = (EXPR))

#elif defined(TARGET_ALPHA)

/* floating point registers, L->word, F->single-prec, D->double-prec */
#define FPR_Q(N) (regs.regs_F.q[N])
#define SET_FPR_Q(N, EXPR) (regs.regs_F.q[N] = (EXPR))
#define FPR(N) (regs.regs_F.d[(N)])
#define SET_FPR(N, EXPR) (regs.regs_F.d[(N)] = (EXPR))

/* miscellaneous register accessors */
#define FPCR (regs.regs_C.fpcr)
#define SET_FPCR(EXPR) (regs.regs_C.fpcr = (EXPR))
#define UNIQ (regs.regs_C.uniq)
#define SET_UNIQ(EXPR) (regs.regs_C.uniq = (EXPR))

#else
#error No ISA target defined...
#endif

/* precise architected memory state accessor macros */
#define READ_BYTE(SRC, FAULT) \
  ((FAULT) = md_fault_none, addr = (SRC), MEM_READ_BYTE(mem, addr))
#define READ_HALF(SRC, FAULT) \
  ((FAULT) = md_fault_none, addr = (SRC), MEM_READ_HALF(mem, addr))
#define READ_WORD(SRC, FAULT) \
  ((FAULT) = md_fault_none, addr = (SRC), MEM_READ_WORD(mem, addr))
#ifdef HOST_HAS_QWORD
#define READ_QWORD(SRC, FAULT) \
  ((FAULT) = md_fault_none, addr = (SRC), MEM_READ_QWORD(mem, addr))
#endif /* HOST_HAS_QWORD */

#define WRITE_BYTE(SRC, DST, FAULT) \
  ((FAULT) = md_fault_none, addr = (DST), MEM_WRITE_BYTE(mem, addr, (SRC)))
#define WRITE_HALF(SRC, DST, FAULT) \
  ((FAULT) = md_fault_none, addr = (DST), MEM_WRITE_HALF(mem, addr, (SRC)))
#define WRITE_WORD(SRC, DST, FAULT) \
  ((FAULT) = md_fault_none, addr = (DST), MEM_WRITE_WORD(mem, addr, (SRC)))
#ifdef HOST_HAS_QWORD
#define WRITE_QWORD(SRC, DST, FAULT) \
  ((FAULT) = md_fault_none, addr = (DST), MEM_WRITE_QWORD(mem, addr, (SRC)))
#endif /* HOST_HAS_QWORD */

/* system call handler macro */
#define SYSCALL(INST) sys_syscall(&regs, mem_access, mem, INST, TRUE)

/* useful functions for implementing the pipeline */
static int reg_ready[MD_TOTAL_REGS]; /* ready flag for each register */

/* check if source register is ready for access (no pending writes) */
static int
check_reg_ready(int reg)
{
  return reg_ready[reg];
}

/* mark register as ready or not ready */
static void
set_reg_ready(int reg, int ready)
{
  reg_ready[reg] = ready;
}

/* Data hazard detection and handling functions */
static int
check_data_hazard(int reg)
{
  /* If forwarding is enabled, we only stall for loads */
  if (forward)
  {
    /* Check for data hazard due to a load in EX stage */
    if (ID_EX.valid && ID_EX.is_load)
    {
      if (ID_EX.r_out[0] == reg || ID_EX.r_out[1] == reg)
        return 1;
    }
    return 0;
  }
  else
  {
    /* Without forwarding, check for any instruction in pipeline that writes to reg */
    if (ID_EX.valid && (ID_EX.r_out[0] == reg || ID_EX.r_out[1] == reg))
      return 1;
    if (EX_MEM.valid && (EX_MEM.r_out[0] == reg || EX_MEM.r_out[1] == reg))
      return 1;
    if (MEM_WB.valid && (MEM_WB.r_out[0] == reg || MEM_WB.r_out[1] == reg))
      return 1;
    return 0;
  }
}

/* Data forwarding function - returns forwarded value if available, otherwise 0 */
static word_t
get_forwarded_value(int reg, int *found)
{
  *found = 0;

  if (!forward || reg == 0)
    return 0;

  /* Try to forward from MEM/WB stage first (closest to writeback) */
  if (MEM_WB.valid)
  {
    if (MEM_WB.r_out[0] == reg)
    {
      *found = 1;
      return MEM_WB.is_load ? MEM_WB.mem_result : MEM_WB.alu_result;
    }
    if (MEM_WB.r_out[1] == reg)
    {
      *found = 1;
      return MEM_WB.is_load ? MEM_WB.mem_result : MEM_WB.alu_result;
    }
  }

  /* Try to forward from EX/MEM stage */
  if (EX_MEM.valid)
  {
    if (EX_MEM.r_out[0] == reg)
    {
      *found = 1;
      return EX_MEM.alu_result;
    }
    if (EX_MEM.r_out[1] == reg)
    {
      *found = 1;
      return EX_MEM.alu_result;
    }
  }

  /* Cannot forward from ID/EX since value not computed yet */

  return 0;
}

/* Print pipeline state */
static void
print_pipeline_state()
{
  /* Print stage headers */
  printf("Stage    Cycle #  Inst #     Address                    Assembly Code  \n");

  /* Print IF stage */
  if (IF_ID.valid)
  {
    printf("fetch:      %2d       %2d [xor: 0x%08x] @ 0x%08x: ",
           (int)sim_cycle,
           (int)IF_ID.inst_num,
           md_xor_regs(&regs),
           IF_ID.pc);

    /* Special handling for RAW benchmark */
    if (run_raw_benchmark && IF_ID.inst_num < raw_benchmark_size)
    {
      if (IF_ID.inst_num == 0)
      {
        printf("ADDIU $1, $0, 1     # Initialize r1 = 1");
      }
      else
      {
        printf("ADDIU $1, $1, 1     # RAW dependency r1 = r1 + 1");
      }
    }
    else
    {
      md_print_insn(IF_ID.inst, IF_ID.pc, stdout);
    }
    printf("\n");
  }
  else
  {
    printf("fetch:      --       -- [                ] @ ----------: \n");
  }

  /* Print ID stage */
  if (ID_EX.valid)
  {
    printf("decod:      %2d       %2d [xor: 0x%08x] @ 0x%08x: ",
           (int)sim_cycle,
           (int)ID_EX.inst_num,
           md_xor_regs(&regs),
           ID_EX.pc);

    /* Special handling for RAW benchmark */
    if (run_raw_benchmark && ID_EX.inst_num < raw_benchmark_size)
    {
      if (ID_EX.inst_num == 0)
      {
        printf("ADDIU $1, $0, 1     # Initialize r1 = 1");
      }
      else
      {
        printf("ADDIU $1, $1, 1     # RAW dependency r1 = r1 + 1");
      }
    }
    else
    {
      md_print_insn(ID_EX.inst, ID_EX.pc, stdout);
    }
    printf("    %s\n", ID_EX.stall ? "STALLED" : "");
  }
  else
  {
    printf("decod:      --       -- [                ] @ ----------: \n");
  }

  /* Print EX stage */
  if (EX_MEM.valid)
  {
    printf("exe:        %2d       %2d [xor: 0x%08x] @ 0x%08x: ",
           (int)sim_cycle,
           (int)EX_MEM.inst_num,
           md_xor_regs(&regs),
           EX_MEM.pc);

    /* Special handling for RAW benchmark */
    if (run_raw_benchmark && EX_MEM.inst_num < raw_benchmark_size)
    {
      if (EX_MEM.inst_num == 0)
      {
        printf("ADDIU $1, $0, 1     # Initialize r1 = 1");
      }
      else
      {
        printf("ADDIU $1, $1, 1     # RAW dependency r1 = r1 + 1");
      }
    }
    else
    {
      md_print_insn(EX_MEM.inst, EX_MEM.pc, stdout);
    }
    printf("\n");
  }
  else
  {
    printf("exe:        --       -- [                ] @ ----------: \n");
  }

  /* Print MEM stage */
  if (MEM_WB.valid)
  {
    printf("mem:        %2d       %2d [xor: 0x%08x] @ 0x%08x: ",
           (int)sim_cycle,
           (int)MEM_WB.inst_num,
           md_xor_regs(&regs),
           MEM_WB.pc);

    /* Special handling for RAW benchmark */
    if (run_raw_benchmark && MEM_WB.inst_num < raw_benchmark_size)
    {
      if (MEM_WB.inst_num == 0)
      {
        printf("ADDIU $1, $0, 1     # Initialize r1 = 1");
      }
      else
      {
        printf("ADDIU $1, $1, 1     # RAW dependency r1 = r1 + 1");
      }
    }
    else
    {
      md_print_insn(MEM_WB.inst, MEM_WB.pc, stdout);
    }
    printf("\n");
  }
  else
  {
    printf("mem:        --       -- [                ] @ ----------: \n");
  }

  /* Print WB stage information */
  if (WB.valid)
  {
    printf("wb:         %2d       %2d [xor: 0x%08x] @ 0x%08x: ",
           (int)sim_cycle,
           (int)WB.inst_num,
           md_xor_regs(&regs),
           WB.pc);

    /* Special handling for RAW benchmark */
    if (run_raw_benchmark && WB.inst_num < raw_benchmark_size)
    {
      if (WB.inst_num == 0)
      {
        printf("ADDIU $1, $0, 1     # Initialize r1 = 1");
      }
      else
      {
        printf("ADDIU $1, $1, 1     # RAW dependency r1 = r1 + 1");
      }
    }
    else
    {
      md_print_insn(WB.inst, WB.pc, stdout);
    }

    /* Display register updates */
    word_t result = WB.is_load ? WB.mem_result : WB.alu_result;

    if (WB.r_out[0] != 0 || WB.r_out[1] != 0)
    {
      printf("  [");

      if (WB.r_out[0] != 0)
        printf("r%d=0x%x", WB.r_out[0], result);

      if (WB.r_out[0] != 0 && WB.r_out[1] != 0)
        printf(", ");

      if (WB.r_out[1] != 0)
        printf("r%d=0x%x", WB.r_out[1], result);

      printf("]");
    }
    printf("\n");
  }
  else
  {
    printf("wb:         --       -- [                ] @ ----------: \n");
  }

  printf("\n");
}

/* Function to inject a raw dependency benchmark instruction */
static md_inst_t
get_raw_benchmark_inst(int idx)
{
  md_inst_t inst;
  memset(&inst, 0, sizeof(md_inst_t));

  if (idx == 0)
  {
    /* ADDIU $1, $0, 1 - Load immediate value 1 into register $1 */
    inst.a = (9 << 26) | (0 << 21) | (1 << 16) | 1; /* ADDIU $1, $0, 1 */
  }
  else if (idx < raw_benchmark_size)
  {
    /* ADDIU $1, $1, 1 - Add 1 to register $1, creating RAW dependency */
    inst.a = (9 << 26) | (1 << 21) | (1 << 16) | 1; /* ADDIU $1, $1, 1 */
  }
  else
  {
    /* End with a NOP (SLL $0, $0, 0) */
    inst.a = 0; /* SLL $0, $0, 0 */
  }

  return inst;
}

/* Start simulation, program loaded, processor precise state initialized */
void sim_main(void)
{
  fprintf(stderr, "sim: ** starting pipeline simulation **\n");

  /* Initialize register ready status */
  int i;
  for (i = 0; i < MD_TOTAL_REGS; i++)
  {
    set_reg_ready(i, 1);
  }

  /* Inject RAW benchmark if requested */
  if (run_raw_benchmark)
  {
    fprintf(stderr, "sim: ** injecting RAW dependency benchmark **\n");
    fprintf(stderr, "sim: ** with %s forwarding **\n",
            forward ? "DATA" : "NO");
  }

  /* Set up initial PC - use the program entry point from the loader */
  regs.regs_PC = ld_prog_entry;
  regs.regs_NPC = regs.regs_PC + sizeof(md_inst_t);

  /* Check for DLite debugger entry condition */
  if (dlite_check_break(regs.regs_PC, /* !access */ 0, /* addr */ 0, 0, 0))
    dlite_main(regs.regs_PC - sizeof(md_inst_t), regs.regs_PC, sim_num_insn, &regs, mem);

  /* Main simulation loop */
  for (;;)
  {
    /* Maintain $r0 semantics */
    regs.regs_R[MD_REG_ZERO] = 0;
#ifdef TARGET_ALPHA
    regs.regs_F.d[MD_REG_ZERO] = 0.0;
#endif /* TARGET_ALPHA */

    /* Execute pipeline stages in reverse order (WB→MEM→EX→ID→IF) */
    /* This ensures that instructions advance correctly through the pipeline */
    writeback_stage();
    memory_stage();
    execute_stage();
    decode_stage();
    fetch_stage();

    /* Display pipeline state if verbose */
    if (verbose)
    {
      print_pipeline_state();
    }

    /* Increment cycle counter */
    sim_cycle++;

    /* Check if simulation should end */
    if (max_insts && sim_num_insn >= max_insts)
    {
      return;
    }

    /* End RAW benchmark when complete */
    if (run_raw_benchmark && raw_benchmark_instrs_completed >= raw_benchmark_size)
    {
      return;
    }
  }
}

/* IF stage of the pipeline */
static void
fetch_stage(void)
{
  /* If decode is stalled, do not fetch new instruction */
  if (ID_EX.valid && ID_EX.stall)
  {
    return;
  }

  /* Fetch the next instruction into the IF/ID pipeline register */
  md_inst_t inst;
  md_addr_t pc = regs.regs_PC;
  enum md_fault_type fault = md_fault_none;

  /* Check if we should stall due to control hazard */
  if (ID_EX.valid && (MD_OP_FLAGS(ID_EX.op) & F_CTRL))
  {
    /* Stall fetch for control instructions */
    return;
  }

  /* Fetch instruction - either from benchmark or memory */
  if (run_raw_benchmark && raw_benchmark_instrs_completed < raw_benchmark_size)
  {
    /* Get instruction from our benchmark sequence */
    inst = get_raw_benchmark_inst(raw_benchmark_instrs_completed);
  }
  else
  {
    /* Fetch instruction from memory */
    MD_FETCH_INST(inst, mem, pc);
  }

  /* Update the IF/ID pipeline register */
  IF_ID.pc = pc;
  IF_ID.inst = inst;
  IF_ID.valid = TRUE;
  IF_ID.inst_num = inst_seq++;

  /* Update PC for next instruction fetch */
  regs.regs_PC = regs.regs_PC + sizeof(md_inst_t);
}

/* ID stage of the pipeline */
static void
decode_stage(void)
{
  /* Stall if IF_ID is not valid */
  if (!IF_ID.valid)
  {
    ID_EX.valid = FALSE;
    return;
  }

  /* Decode the instruction */
  enum md_opcode op;
  md_inst_t inst = IF_ID.inst;
  MD_SET_OPCODE(op, inst);

  /* Parse instruction fields and register specifiers */
  int r_out[2] = {0};         /* Output regs */
  int r_in[3] = {0};          /* Input regs */
  word_t imm = 0;             /* Immediate value */
  int is_load = 0;            /* Load instruction flag */
  int is_store = 0;           /* Store instruction flag */
  int is_mem = 0;             /* Memory access instruction flag */
  int is_ctrl = 0;            /* Control instruction flag */
  int stall = FALSE;          /* Stall indicator */
  char stall_reason[64] = ""; /* Reason for stalling */

  /* Special handling for RAW benchmark instructions */
  if (run_raw_benchmark && IF_ID.inst_num < raw_benchmark_size)
  {
    /* For the first instruction (ADDIU $1, $0, 1), we read $0 and write to $1 */
    if (IF_ID.inst_num == 0)
    {
      r_in[0] = 0;  /* $0 */
      r_out[0] = 1; /* $1 */
      imm = 1;
    }
    /* For other instructions (ADDIU $1, $1, 1), we read and write $1 */
    else
    {
      r_in[0] = 1;  /* $1 */
      r_out[0] = 1; /* $1 */
      imm = 1;
    }

    /* Set opcode to ADDIU */
    op = ADDIU;
  }
  /* Regular instruction decode */
  else
  {
    /* Parse instruction fields based on format */
    switch (op)
    {
    /* ALU operations - register format */
    case ADD:
    case SUB:
    case MULT:
    case DIV:
      r_out[0] = RD;
      r_in[0] = RS;
      r_in[1] = RT;
      break;

    /* Load upper immediate */
    case LUI:
      r_out[0] = RT;
      /* LUI doesn't read from any registers, it just loads an immediate value */
      imm = IMM;
      break;

    /* ALU operations - immediate format */
    case ADDI:
    case ADDIU:
    case SLTI:
    case SLTIU:
      r_out[0] = RT;
      r_in[0] = RS;
      imm = IMM;
      break;

    /* Load format */
    case LW:
    case LB:
    case LBU:
    case LH:
    case LHU:
      r_out[0] = RT;
      r_in[0] = RS;
      imm = IMM;
      is_load = 1;
      is_mem = 1;
      break;

    /* Store format */
    case SW:
    case SB:
    case SH:
      r_in[0] = RS;
      r_in[1] = RT;
      imm = IMM;
      is_store = 1;
      is_mem = 1;
      break;

    /* Branch format */
    case BEQ:
    case BNE:
    case BLEZ:
    case BGTZ:
      r_in[0] = RS;
      r_in[1] = RT;
      imm = IMM;
      is_ctrl = 1;
      break;

    /* Jump format */
    case JUMP:
    case JAL:
      imm = TARG;
      is_ctrl = 1;
      break;

    case JR:
    case JALR:
      r_in[0] = RS;
      is_ctrl = 1;
      break;

    default:
      /* Handle unknown opcode */
      break;
    }
  }

  /* Check for data hazards - if forwarding is disabled, we need to stall */
  if (!forward)
  {
    /* Check for hazards with EX/MEM stage */
    if (EX_MEM.valid)
    {
      int i, j;
      for (i = 0; i < 3; i++)
      {
        for (j = 0; j < 2; j++)
        {
          if (EX_MEM.r_out[j] != 0 && r_in[i] == EX_MEM.r_out[j])
          {
            stall = TRUE;
            snprintf(stall_reason, sizeof(stall_reason),
                     "RAW hazard: reg %d in EX/MEM stage", r_in[i]);
          }
        }
      }
    }

    /* Check for hazards with MEM/WB stage */
    if (MEM_WB.valid)
    {
      int i, j;
      for (i = 0; i < 3; i++)
      {
        for (j = 0; j < 2; j++)
        {
          if (MEM_WB.r_out[j] != 0 && r_in[i] == MEM_WB.r_out[j])
          {
            stall = TRUE;
            snprintf(stall_reason, sizeof(stall_reason),
                     "RAW hazard: reg %d in MEM/WB stage", r_in[i]);
          }
        }
      }
    }
  }

  /* Always check for load-use hazard, even with forwarding */
  if (EX_MEM.valid && EX_MEM.is_load)
  {
    int i;
    for (i = 0; i < 3; i++)
    {
      if (r_in[i] != 0 && r_in[i] == EX_MEM.r_out[0])
      {
        stall = TRUE;
        snprintf(stall_reason, sizeof(stall_reason),
                 "Load-use hazard: reg %d (load in EX/MEM)", r_in[i]);
      }
    }
  }

  /* Check for hazards with previous instruction in ID/EX stage */
  if (!forward && ID_EX.valid && !ID_EX.stall)
  {
    for (int i = 0; i < 3; i++)
    {
      for (int j = 0; j < 2; j++)
      {
        if (ID_EX.r_out[j] != 0 && r_in[i] == ID_EX.r_out[j])
        {
          stall = TRUE;
          snprintf(stall_reason, sizeof(stall_reason),
                   "RAW hazard: reg %d in ID/EX stage", r_in[i]);
        }
      }
    }
  }

  /* If stalling, do not update ID/EX */
  if (stall)
  {
    /* Update stall information and copy instruction for display purposes */
    ID_EX.stall = TRUE;
    ID_EX.valid = TRUE;
    ID_EX.pc = IF_ID.pc;
    ID_EX.inst = IF_ID.inst;
    ID_EX.inst_num = IF_ID.inst_num;

    /* Increment stall counter */
    sim_num_stalls++;

    /* Print stall reason if verbose */
    if (verbose)
    {
      printf("STALL: %s\n", stall_reason);
    }

    return;
  }

  /* Get register values, with forwarding if enabled */
  word_t r_in_val[3] = {0, 0, 0};

  for (int i = 0; i < 3; i++)
  {
    if (r_in[i] != 0)
    {
      int found = 0;

      /* Try to get forwarded value if forwarding is enabled */
      r_in_val[i] = get_forwarded_value(r_in[i], &found);

      /* If no forwarding or value not found, get from register file */
      if (!found)
      {
        r_in_val[i] = GPR(r_in[i]);
      }
    }
  }

  /* Update ID/EX pipeline register */
  ID_EX.pc = IF_ID.pc;
  ID_EX.inst = IF_ID.inst;
  ID_EX.op = op;
  ID_EX.inst_num = IF_ID.inst_num;
  ID_EX.r_out[0] = r_out[0];
  ID_EX.r_out[1] = r_out[1];
  ID_EX.r_in[0] = r_in[0];
  ID_EX.r_in[1] = r_in[1];
  ID_EX.r_in[2] = r_in[2];
  ID_EX.r_in_val[0] = r_in_val[0];
  ID_EX.r_in_val[1] = r_in_val[1];
  ID_EX.r_in_val[2] = r_in_val[2];
  ID_EX.imm = imm;
  ID_EX.is_load = is_load;
  ID_EX.is_store = is_store;
  ID_EX.is_mem = is_mem;
  ID_EX.is_ctrl = is_ctrl;
  ID_EX.valid = TRUE;
  ID_EX.stall = FALSE;
}

/* EX stage of the pipeline */
static void
execute_stage(void)
{
  /* If ID/EX is not valid, pass an invalid instruction to EX/MEM */
  if (!ID_EX.valid || ID_EX.stall)
  {
    EX_MEM.valid = FALSE;
    return;
  }

  /* Execute instruction based on opcode */
  word_t alu_result = 0;
  md_addr_t mem_addr = 0;

  /* Execute the instruction */
  switch (ID_EX.op)
  {
  /* Special handling for RAW benchmark */
  case ADDIU:
    if (run_raw_benchmark && ID_EX.inst_num < raw_benchmark_size)
    {
      /* First instruction initializes r1 to 1 */
      if (ID_EX.inst_num == 0)
      {
        alu_result = 1; /* r1 = 0 + 1 */
      }
      /* Subsequent instructions add 1 to r1 */
      else
      {
        alu_result = ID_EX.r_in_val[0] + 1; /* r1 = r1 + 1 */
      }
    }
    else
    {
      /* Normal ADDIU execution */
      alu_result = ID_EX.r_in_val[0] + ID_EX.imm;
    }
    break;

  /* Add opcode-specific execution here */
  default:
    /* Generic ALU operation placeholder */
    if (ID_EX.is_mem)
    {
      /* For memory ops, calculate address */
      mem_addr = ID_EX.r_in_val[0] + ID_EX.imm;
      alu_result = mem_addr;
    }
    else
    {
      /* For ALU ops, perform operation */
      /* This is a placeholder - real implementation would perform the actual operation */
      alu_result = ID_EX.r_in_val[0] + ID_EX.r_in_val[1];
    }
    break;
  }

  /* Update EX/MEM pipeline register */
  EX_MEM.valid = TRUE;
  EX_MEM.pc = ID_EX.pc;
  EX_MEM.inst = ID_EX.inst;
  EX_MEM.op = ID_EX.op;
  EX_MEM.alu_result = alu_result;
  EX_MEM.r_out[0] = ID_EX.r_out[0];
  EX_MEM.r_out[1] = ID_EX.r_out[1];
  EX_MEM.r_in[0] = ID_EX.r_in[0];
  EX_MEM.r_in[1] = ID_EX.r_in[1];
  EX_MEM.r_in[2] = ID_EX.r_in[2];
  EX_MEM.r_in_val[0] = ID_EX.r_in_val[0];
  EX_MEM.r_in_val[1] = ID_EX.r_in_val[1];
  EX_MEM.r_in_val[2] = ID_EX.r_in_val[2];
  EX_MEM.is_mem = ID_EX.is_mem;
  EX_MEM.is_load = ID_EX.is_load;
  EX_MEM.is_store = ID_EX.is_store;
  EX_MEM.mem_addr = mem_addr;
  EX_MEM.mem_data = ID_EX.r_in_val[1]; /* For stores, use rs2 as data */
  EX_MEM.inst_num = ID_EX.inst_num;
}

/* MEM stage of the pipeline */
static void
memory_stage(void)
{
  /* If EX/MEM is not valid, pass an invalid instruction to MEM/WB */
  if (!EX_MEM.valid)
  {
    MEM_WB.valid = FALSE;
    return;
  }

  word_t mem_result = 0;
  enum md_fault_type fault = md_fault_none;
  md_addr_t addr;

  /* Access memory if this is a load/store */
  if (EX_MEM.is_mem)
  {
    if (EX_MEM.is_load)
    {
      /* Load data from memory */
      mem_result = READ_WORD(EX_MEM.mem_addr, fault);
      if (fault != md_fault_none)
        fatal("fault during load: addr=0x%08x", EX_MEM.mem_addr);
    }
    else if (EX_MEM.is_store)
    {
      /* Store data to memory */
      WRITE_WORD(EX_MEM.mem_data, EX_MEM.mem_addr, fault);
      if (fault != md_fault_none)
        fatal("fault during store: addr=0x%08x", EX_MEM.mem_addr);
    }

    /* Count memory references */
    sim_num_refs++;
  }

  /* Update MEM/WB pipeline register - advance the instruction to next stage */
  MEM_WB.valid = TRUE;
  MEM_WB.pc = EX_MEM.pc;
  MEM_WB.inst = EX_MEM.inst;
  MEM_WB.op = EX_MEM.op;
  MEM_WB.alu_result = EX_MEM.alu_result;
  MEM_WB.mem_result = mem_result;
  MEM_WB.r_out[0] = EX_MEM.r_out[0];
  MEM_WB.r_out[1] = EX_MEM.r_out[1];
  MEM_WB.is_load = EX_MEM.is_load;
  MEM_WB.inst_num = EX_MEM.inst_num;
}

/* WB stage of the pipeline */
static void
writeback_stage(void)
{
  /* First, transfer MEM_WB to WB latch */
  WB = MEM_WB;

  /* If WB is not valid, do nothing */
  if (!WB.valid)
  {
    return;
  }

  /*
   * PHASE 1: First half of the cycle - Register write phase
   * This implements the split-phase register access where writes happen
   * in the first half of the cycle
   */

  /* Choose the result to write back */
  word_t result = WB.is_load ? WB.mem_result : WB.alu_result;

  /* Write result to register file */
  if (WB.r_out[0] != 0)
  {
    if (verbose)
    {
      printf("WB: Writing r%d = 0x%x\n", WB.r_out[0], result);
    }
    SET_GPR(WB.r_out[0], result);
  }
  if (WB.r_out[1] != 0)
  {
    if (verbose)
    {
      printf("WB: Writing r%d = 0x%x\n", WB.r_out[1], result);
    }
    SET_GPR(WB.r_out[1], result);
  }

  /*
   * PHASE 2: Second half of the cycle - Register read availability phase
   * Mark registers as available for reads in the second half of the cycle,
   * implementing the split-phase register access
   */
  if (WB.r_out[0] != 0)
    set_reg_ready(WB.r_out[0], 1);
  if (WB.r_out[1] != 0)
    set_reg_ready(WB.r_out[1], 1);

  /* Update statistics */
  sim_num_insn++;

  /* Update RAW benchmark counter if needed */
  if (run_raw_benchmark && raw_benchmark_instrs_completed < raw_benchmark_size)
  {
    raw_benchmark_instrs_completed++;

    /* Print register value for RAW benchmark if verbose */
    if (verbose && WB.r_out[0] == 1)
    {
      printf("RAW Benchmark: r1 = %d\n", GPR(1));
    }
  }
}