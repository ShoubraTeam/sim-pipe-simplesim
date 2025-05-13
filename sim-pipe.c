/* sim-pipe.c - pipeline simulator implementation */

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
 * This file implements a 5-stage pipeline simulator.
 * The simulator models the following assumptions:
 *  • Perfect instruction cache (no I-cache misses)
 *  • Perfect data cache (no D-cache misses)
 *  • No branch prediction
 *  • Split-phase register access for the writeback (WB) stage.
 *    This means that writes to registers occur in the first half
 *    of the clock cycle, and reads occur in the second half.
 */

/* simulated registers */
static struct regs_t regs;

/* simulated memory */
static struct mem_t *mem = NULL;

/* track number of refs */
static counter_t sim_num_refs = 0;

/* maximum number of inst's to execute */
static unsigned int max_insts;

/* pipeline stage latch names */
#define F_STAGE 0
#define D_STAGE 1
#define E_STAGE 2
#define M_STAGE 3
#define W_STAGE 4

/* total number of pipeline stages */
#define PIPE_STAGES 5

/* pipeline stage names for output */
static char *stage_names[PIPE_STAGES] = {"fetch", "decod", "exec ", "mem  ", "wb   "};

/* Boolean values */
#define TRUE 1
#define FALSE 0

/* pipeline latch */
struct pipeline_latch
{
  /* latch contents */
  md_inst_t inst; /* instruction */
  md_addr_t pc;   /* program counter */
  md_addr_t npc;  /* next program counter */
  int op;         /* decoded instruction opcode */
  md_addr_t addr; /* computed memory address */
  int result_reg; /* result register */
  word_t val_a;   /* operand a value */
  word_t val_b;   /* operand b value */
  word_t val_c;   /* operand c value */
  word_t result;  /* execution result */

  /* latch status */
  int valid;    /* valid bit */
  int stall;    /* stall bit */
  int is_mem;   /* memory instruction bit */
  int is_wb;    /* write-back instruction bit */
  int low_reg;  /* low register number */
  int high_reg; /* high register number */
};

/* all five pipeline stages */
static struct pipeline_latch pipe_latch[PIPE_STAGES];

/* pipe op stage counters */
static counter_t sim_pipe_stages[PIPE_STAGES];

/* register update unit (tracks register updates in current cycle) */
struct reg_update
{
  int reg;    /* register number to update */
  word_t val; /* value to write */
};

#define MAX_REG_UPDATES 8
static int reg_update_count = 0;
static struct reg_update reg_updates[MAX_REG_UPDATES];

/* cycle counter */
static counter_t sim_cycle = 0;

/* instruction count */
static counter_t sim_instr_count = 0;

/* command line options */

/* output options */
static int show_stats = FALSE;
static int disable_forwarding = TRUE; /* default to no forwarding (as required) */
static int quiet_mode = FALSE;        /* quiet mode - suppress headers */

/* initialize a pipeline latch */
static void
init_latch(int stage)
{
  pipe_latch[stage].valid = FALSE;
  pipe_latch[stage].stall = FALSE;
  pipe_latch[stage].is_mem = FALSE;
  pipe_latch[stage].is_wb = FALSE;
}

/* initialize the pipeline */
static void
init_pipeline(void)
{
  int i;
  for (i = 0; i < PIPE_STAGES; i++)
    init_latch(i);
  reg_update_count = 0;
}

/* register simulator-specific options */
void sim_reg_options(struct opt_odb_t *odb)
{
  opt_reg_header(odb,
                 "sim-pipe: This simulator implements a 5-stage pipeline simulator.\n"
                 "The simulator models the following assumptions:\n"
                 "  • Perfect instruction cache (no I-cache misses)\n"
                 "  • Perfect data cache (no D-cache misses)\n"
                 "  • No branch prediction\n"
                 "  • Split-phase register access for the writeback (WB) stage.\n"
                 "    This means that writes to registers occur in the first half\n"
                 "    of the clock cycle, and reads occur in the second half.\n");

  /* output options */
  opt_reg_flag(odb, "-show:stats",
               "output statistics at the end of the simulation",
               &show_stats, /* default */ FALSE,
               /* print */ TRUE, /* format */ NULL);

  /* forwarding option */
  opt_reg_flag(odb, "-disable:forwarding",
               "disable data forwarding in the pipeline",
               &disable_forwarding, /* default */ TRUE,
               /* print */ TRUE, /* format */ NULL);

  /* quiet mode - suppress headers */
  opt_reg_flag(odb, "-quiet",
               "quiet mode - suppress headers",
               &quiet_mode, /* default */ FALSE,
               /* print */ TRUE, /* format */ NULL);

  /* instruction limit */
  opt_reg_uint(odb, "-max:inst", "maximum number of inst's to execute",
               &max_insts, /* default */ 0,
               /* print */ TRUE, /* format */ NULL);
}

/* check simulator-specific option values */
void sim_check_options(struct opt_odb_t *odb, int argc, char **argv)
{
  /* nothing currently */
}

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
                   "total number of cycles",
                   &sim_cycle, 0, NULL);
  stat_reg_formula(sdb, "sim_IPC",
                   "instructions per cycle",
                   "sim_num_insn / sim_num_cycles", NULL);
  stat_reg_counter(sdb, "sim_stall_count",
                   "total number of stall cycles",
                   &sim_pipe_stages[D_STAGE], 0, NULL);
  stat_reg_int(sdb, "sim_elapsed_time",
               "total simulation time in seconds",
               &sim_elapsed_time, 0, NULL);
  stat_reg_formula(sdb, "sim_cycle_rate",
                   "simulation speed (cycles/sec)",
                   "sim_num_cycles / sim_elapsed_time", NULL);
  ld_reg_stats(sdb);
  mem_reg_stats(mem, sdb);
}

/* initialize the simulator */
void sim_init(void)
{
  sim_num_refs = 0;

  /* allocate and initialize register file */
  regs_init(&regs);

  /* allocate and initialize memory space */
  mem = mem_create("mem");
  mem_init(mem);

  /* initialize pipeline */
  init_pipeline();
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
void sim_aux_config(FILE *stream) /* output stream */
{
  /* nothing currently */
}

/* dump simulator-specific auxiliary simulator statistics */
void sim_aux_stats(FILE *stream) /* output stream */
{
  /* nothing currently */
}

/* un-initialize simulator-specific state */
void sim_uninit(void)
{
  /* nothing currently */
}

/* print the pipeline state */
static void
print_pipeline(void)
{
  int i;
  static int header_printed = 0;

  /* Print header only once */
  if (!header_printed && !quiet_mode)
  {
    printf("Stage   Cycle #  Inst #    Address         Assembly Code\n");
    header_printed = 1;
  }

  for (i = 0; i < PIPE_STAGES; i++)
  {
    if (pipe_latch[i].valid)
    {
      printf("%s:  ", stage_names[i]);
      printf("%7d  ", (int)sim_cycle + 1); /* Make cycle counter start from 1 */
      printf("%6d ", i);                   /* Map fetch=0, decode=1, etc. */
      printf("[xor: 0x%08x] ", md_xor_regs(&regs));
#ifdef TARGET_ALPHA
      printf("@ 0x%016llx: ", pipe_latch[i].pc);
#else
      printf("@ 0x%08x: ", (unsigned int)pipe_latch[i].pc);
#endif
      md_print_insn(pipe_latch[i].inst, pipe_latch[i].pc, stdout);
      printf("\n");
    }
  }
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

/* queue a register update for the next cycle */
static void
reg_enqueue(int reg, word_t val)
{
  if (reg_update_count >= MAX_REG_UPDATES)
    panic("register update queue overflow");

  reg_updates[reg_update_count].reg = reg;
  reg_updates[reg_update_count].val = val;
  reg_update_count++;
}

/* apply all queued register updates */
static void
reg_commit(void)
{
  int i;

  for (i = 0; i < reg_update_count; i++)
  {
    SET_GPR(reg_updates[i].reg, reg_updates[i].val);
  }

  /* reset register update queue */
  reg_update_count = 0;
}

/* start simulation, program loaded, processor precise state initialized */
void sim_main(void)
{
  md_inst_t inst;
  register md_addr_t addr;
  enum md_opcode op;
  register int is_write;
  enum md_fault_type fault;

  /* Don't print header here, it's now handled in print_pipeline */

  /* set up initial default next PC */
  regs.regs_NPC = regs.regs_PC + sizeof(md_inst_t);

  /* check for DLite debugger entry condition */
  if (dlite_check_break(regs.regs_PC, /* !access */ 0, /* addr */ 0, 0, 0))
    dlite_main(regs.regs_PC - sizeof(md_inst_t),
               regs.regs_PC, sim_num_insn, &regs, mem);

  /* main simulator loop */
  while (TRUE)
  {
    /* maintain $r0 semantics */
    regs.regs_R[MD_REG_ZERO] = 0;
#ifdef TARGET_ALPHA
    regs.regs_F.d[MD_REG_ZERO] = 0.0;
#endif /* TARGET_ALPHA */

    /* perform writeback (WB) stage - first half of cycle */
    if (pipe_latch[W_STAGE].valid)
    {
      /* commit register file updates in this cycle */
      if (pipe_latch[W_STAGE].is_wb)
        reg_enqueue(pipe_latch[W_STAGE].result_reg, pipe_latch[W_STAGE].result);
    }

    /* apply all register updates - commits all registers */
    reg_commit();

    /* perform memory (MEM) stage */
    if (pipe_latch[M_STAGE].valid && !pipe_latch[M_STAGE].stall)
    {
      pipe_latch[W_STAGE] = pipe_latch[M_STAGE];
      pipe_latch[W_STAGE].valid = TRUE; /* Ensure writeback latch is valid */

      /* access memory if needed */
      if (pipe_latch[M_STAGE].is_mem)
      {
        is_write = (MD_OP_FLAGS(pipe_latch[M_STAGE].op) & F_STORE);
        if (is_write)
        {
          /* store operation */
          addr = pipe_latch[M_STAGE].addr;
          switch (pipe_latch[M_STAGE].op)
          {
#define DEFINST(OP, MSK, NAME, OPFORM, RES, FLAGS, O1, O2, I1, I2, I3) \
  case OP:                                                             \
    /* check for 'write' instructions */                               \
    if (((FLAGS) & F_STORE))                                           \
      SYMCAT(OP, _IMPL);                                               \
    break;
#define DEFLINK(OP, MSK, NAME, MASK, SHIFT) \
  case OP:                                  \
    /* just a NOP */                        \
    break;
#define CONNECT(OP)
#define DECLARE_FAULT(FAULT) \
  {                          \
    fault = (FAULT);         \
    break;                   \
  }
#include "machine.def"
          default:
            /* some kind of error, would normally panic, but just ignore for now */
            break;
          }
        }
        else
        {
          /* load operation */
          addr = pipe_latch[M_STAGE].addr;
          switch (pipe_latch[M_STAGE].op)
          {
#define DEFINST(OP, MSK, NAME, OPFORM, RES, FLAGS, O1, O2, I1, I2, I3) \
  case OP:                                                             \
    /* check for 'load' instructions */                                \
    if (((FLAGS) & F_LOAD))                                            \
      SYMCAT(OP, _IMPL);                                               \
    break;
#define DEFLINK(OP, MSK, NAME, MASK, SHIFT) \
  case OP:                                  \
    /* just a NOP */                        \
    break;
#define CONNECT(OP)
#define DECLARE_FAULT(FAULT) \
  {                          \
    fault = (FAULT);         \
    break;                   \
  }
#include "machine.def"
          default:
            /* some kind of error, would normally panic, but just ignore for now */
            break;
          }
        }
      }
      sim_pipe_stages[M_STAGE]++;
      init_latch(M_STAGE);
    }
    else if (pipe_latch[M_STAGE].valid && pipe_latch[M_STAGE].stall)
    {
      /* stall in memory stage */
      pipe_latch[W_STAGE].valid = FALSE;
    }
    else
    {
      /* pipeline bubble */
      pipe_latch[W_STAGE].valid = FALSE;
    }

    /* perform execute (EX) stage */
    if (pipe_latch[E_STAGE].valid && !pipe_latch[E_STAGE].stall)
    {
      pipe_latch[M_STAGE] = pipe_latch[E_STAGE];
      pipe_latch[M_STAGE].valid = TRUE; /* Ensure memory latch is valid */

      /* perform the ALU operation */
      switch (pipe_latch[E_STAGE].op)
      {
#define DEFINST(OP, MSK, NAME, OPFORM, RES, FLAGS, O1, O2, I1, I2, I3) \
  case OP:                                                             \
    /* compute the result if needed */                                 \
    if ((RES) != NA)                                                   \
      SYMCAT(OP, _IMPL);                                               \
    break;
#define DEFLINK(OP, MSK, NAME, MASK, SHIFT) \
  case OP:                                  \
    /* just a NOP */                        \
    break;
#define CONNECT(OP)
#define DECLARE_FAULT(FAULT) \
  {                          \
    fault = (FAULT);         \
    break;                   \
  }
#include "machine.def"
      default:
        /* some kind of error, would normally panic, but just ignore for now */
        break;
      }

      /* check for branch instructions */
      if (MD_OP_FLAGS(pipe_latch[E_STAGE].op) & F_CTRL)
      {
        md_addr_t target_PC = pipe_latch[E_STAGE].result;

        /* branch taken */
        if (target_PC != pipe_latch[E_STAGE].npc)
        {
          /* flush the pipeline of instructions from the wrong path */
          init_latch(F_STAGE);
          init_latch(D_STAGE);

          /* restart fetch from the target address */
          regs.regs_PC = target_PC;
          regs.regs_NPC = target_PC + sizeof(md_inst_t);
        }
      }

      sim_pipe_stages[E_STAGE]++;
      init_latch(E_STAGE);
    }
    else if (pipe_latch[E_STAGE].valid && pipe_latch[E_STAGE].stall)
    {
      /* stall in execute stage */
      pipe_latch[M_STAGE].valid = FALSE;
    }
    else
    {
      /* pipeline bubble */
      pipe_latch[M_STAGE].valid = FALSE;
    }

    /* perform decode (ID) stage */
    if (pipe_latch[D_STAGE].valid && !pipe_latch[D_STAGE].stall)
    {
      pipe_latch[E_STAGE] = pipe_latch[D_STAGE];

      /* decode the instruction */
      MD_SET_OPCODE(op, pipe_latch[D_STAGE].inst);
      pipe_latch[E_STAGE].op = op;

      /* resolve source/target register values */
      /* no forwarding in this model - stall on hazards */
      /* FIXME: insert code to detect hazards and provide stall/forward logic */

      /* Added code for hazard detection and stalling */
      if (disable_forwarding)
      {
        int stall_needed = 0;

        /* Get source registers for current instruction */
        int rs = RS;
        int rt = RT;

        /* Get destination registers for instructions in EX, MEM, and WB stages */
        int ex_rd = (pipe_latch[E_STAGE].valid) ? ((pipe_latch[E_STAGE].inst.b >> 8) & 0xff) : -1;
        int mem_rd = (pipe_latch[M_STAGE].valid) ? ((pipe_latch[M_STAGE].inst.b >> 8) & 0xff) : -1;
        int wb_rd = (pipe_latch[W_STAGE].valid) ? ((pipe_latch[W_STAGE].inst.b >> 8) & 0xff) : -1;

        /* Check for RAW hazards */
        if (MD_OP_FLAGS(op) & (F_ICOMP | F_FCOMP | F_MEM | F_CTRL))
        {
          /* RS dependency check */
          if (rs != 0 && (rs == ex_rd || rs == mem_rd || rs == wb_rd))
          {
            stall_needed = 1;
          }

          /* RT dependency check (if used as source) */
          if (!stall_needed && rt != 0 &&
              !(MD_OP_FLAGS(op) & (F_STORE | F_CTRL)) &&
              (rt == ex_rd || rt == mem_rd || rt == wb_rd))
          {
            stall_needed = 1;
          }
        }

        if (stall_needed)
        {
          /* Stall the pipeline */
          pipe_latch[D_STAGE].stall = 1;
          pipe_latch[E_STAGE].valid = 0;
          return; /* Skip the rest of this cycle */
        }
      }

      /* no branch prediction - assume not taken */

      sim_pipe_stages[D_STAGE]++;
      init_latch(D_STAGE);
    }
    else if (pipe_latch[D_STAGE].valid && pipe_latch[D_STAGE].stall)
    {
      /* stall in decode stage */
      pipe_latch[E_STAGE].valid = FALSE;
    }
    else
    {
      /* pipeline bubble */
      pipe_latch[E_STAGE].valid = FALSE;
    }

    /* perform fetch (IF) stage */
    if (pipe_latch[F_STAGE].valid && !pipe_latch[F_STAGE].stall)
    {
      /* instruction available in F-stage */
      pipe_latch[D_STAGE] = pipe_latch[F_STAGE];

      sim_pipe_stages[F_STAGE]++;
      init_latch(F_STAGE);

      /* fetch the next instruction */
      pipe_latch[F_STAGE].pc = regs.regs_PC;
      pipe_latch[F_STAGE].npc = regs.regs_NPC;

      /* get the next instruction */
      MD_FETCH_INST(inst, mem, regs.regs_PC);
      pipe_latch[F_STAGE].inst = inst;
      pipe_latch[F_STAGE].valid = TRUE;

      /* go to the next instruction */
      regs.regs_PC = regs.regs_NPC;
      regs.regs_NPC += sizeof(md_inst_t);
    }
    else if (pipe_latch[F_STAGE].valid && pipe_latch[F_STAGE].stall)
    {
      /* stall in fetch stage */
      pipe_latch[D_STAGE].valid = FALSE;
    }
    else
    {
      /* start up the pipeline with first instruction fetch */
      pipe_latch[D_STAGE].valid = FALSE;
      pipe_latch[F_STAGE].pc = regs.regs_PC;
      pipe_latch[F_STAGE].npc = regs.regs_NPC;

      /* get the first instruction */
      MD_FETCH_INST(inst, mem, regs.regs_PC);
      pipe_latch[F_STAGE].inst = inst;
      pipe_latch[F_STAGE].valid = TRUE;

      /* go to the next instruction */
      regs.regs_PC = regs.regs_NPC;
      regs.regs_NPC += sizeof(md_inst_t);
    }

    /* print pipeline status */
    print_pipeline();

    /* check for DLite debugger entry condition */
    if (dlite_check_break(regs.regs_NPC,
                          is_write ? ACCESS_WRITE : ACCESS_READ,
                          addr, sim_num_insn, sim_num_insn))
      dlite_main(regs.regs_PC, regs.regs_NPC, sim_num_insn, &regs, mem);

    /* check for a completed instruction */
    if (pipe_latch[W_STAGE].valid)
    {
      sim_num_insn++;

      /* count instruction with memory access */
      if (pipe_latch[W_STAGE].is_mem)
        sim_num_refs++;
    }

    /* increment cycle counter */
    sim_cycle++;

    /* finish early? */
    if (max_insts && sim_num_insn >= max_insts)
    {
      if (show_stats)
        return;
      else
        exit(0);
    }
  }
}