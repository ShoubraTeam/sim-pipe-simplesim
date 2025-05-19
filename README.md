# Understanding how pipelines function using SimpleScalar

## Overview
A simulation project that models and analyzes **how different pipeline stages work and their impact on processor performance (with & without forwarding)**, with a focus on understanding and solving challenges that arose during development.

## Course Details and Team Members
- **University**: Benha University
- **College**: Shoubra Faculty of Engineering
- **Department**: Computer Engineering
- **Course**: Computer Architecture
- **Professor**: Dr. May Salama
- **Project**: Understanding how pipelines function using SimpleScalar
- **Team Members**:
  - Karim Wael
  - Fady Youssef
  - Abdelrahman Osman
  - Hanin Mustafa
  - Sara Alaa

## 📚 Table of Contents

- [💡 Acknowledgments](#-acknowledgments)
- [🔍 Problem Description](#-problem-description)
- [🧰 Prerequisites](#-prerequisites)
- [🖥️ Environment Setup](#-environment-setup)
- [✏️ Simulation Code Implementation](#-simulation-code-implementation)
- [🏁 Compile and Run](#-compile-and-run)
- [🧪 Testing and Validation](#-testing-and-validation)
- [🧩 Challenges Encountered](#-challenges-encountered)
- [🛠️ Handling Challenges](#-handling-challenges)
- [📝 Conclusion](#-conclusion)
- [📌 References](#-references)
- [👥 Contributors](#-contributors)

## 💡 Acknowledgments
We would like to express our gratitude to Dr. May Salama for her guidance and support throughout this project. We also acknowledge the SimpleScalar team for providing the simulation tools that made this project possible.

## 🔍 Problem Description
The project aims to demonstrate and analyze pipeline functionality in modern processors through simulation. By using SimpleScalar, we explore:
- Basic pipeline stages (Fetch, Decode, Execute, Memory, Writeback)
- Data hazards and their impact on performance
- The effectiveness of forwarding techniques
- Performance metrics and optimization strategies

## 🧰 Prerequisites
- Linux-based operating system
- GCC compiler (version 7.0 or higher)
- Make utility
- Basic understanding of computer architecture concepts
- Familiarity with C programming language

## 🖥️ Environment Setup
1. Install required dependencies:
   ```bash
   sudo apt-get update
   sudo apt-get install build-essential gcc make
   ```
2. Download and extract [SimpleScalar v3.0](https://github.com/toddmaustin/simplesim-3.0):
   ```bash
   git clone https://github.com/toddmaustin/simplesim-3.0.git
   ```
3. Navigate to SimpleScalar folder:
   ```bash
   cd simplesim-3.0
   ```
4. Configure the build environment:
   ```bash
   make config-pisa
   make
   ```

## ✏️ Simulation Code Implementation
The simulation is implemented using SimpleScalar's pipeline simulator (sim-pipe2). Key features include:
- Configurable pipeline stages
- Forwarding path implementation
- Performance monitoring
- Detailed instruction tracking

### Key Changes from sim-safe to sim-pipe2

1. **Pipeline Structure Implementation**
```c
/* Pipeline stage definitions */
#define F_STAGE 0  // Fetch
#define D_STAGE 1  // Decode
#define E_STAGE 2  // Execute
#define M_STAGE 3  // Memory
#define W_STAGE 4  // Writeback

/* Pipeline registers between stages */
typedef struct ifid_buf_t {
    md_inst_t inst;     /* instruction */
    md_addr_t pc;       /* program counter */
    int valid;          /* valid bit */
    counter_t inst_num; /* instruction sequence number */
} ifid_buf_t;

/* Similar structures for ID/EX, EX/MEM, and MEM/WB stages */
```

2. **Forwarding Implementation**
```c
/* Forwarding control */
static int forward;  // Command line option for forwarding

/* Register ready status tracking */
static void set_reg_ready(int reg, int status) {
    reg_ready[reg] = status;
}

/* Forwarding logic in decode stage */
word_t get_forwarded_value(int reg, int *found) {
    if (forward) {
        // Check EX stage
        if (EX_MEM.valid && EX_MEM.r_out[0] == reg) {
            *found = 1;
            return EX_MEM.alu_result;
        }
        // Check MEM stage
        if (MEM_WB.valid && MEM_WB.r_out[0] == reg) {
            *found = 1;
            return MEM_WB.alu_result;
        }
    }
    *found = 0;
    return 0;
}
```

3. **Pipeline Control and Stalling**
```c
/* Stall detection in decode stage */
static void decode_stage(void) {
    int stall = FALSE;
    char *stall_reason = NULL;
    
    // Check for data hazards
    if (!disable_forwarding) {
        for (int i = 0; i < 3; i++) {
            if (r_in[i] != 0 && !reg_ready[r_in[i]]) {
                stall = TRUE;
                stall_reason = "Data hazard";
                break;
            }
        }
    }
    
    // Handle stall
    if (stall) {
        ID_EX.stall = TRUE;
        sim_num_stalls++;
    }
}
```

4. **Split-Phase Register Access**
```c
/* Writeback stage with split-phase access */
static void writeback_stage(void) {
    /* First half of cycle - Register writes */
    if (WB.valid) {
        word_t result = WB.is_load ? WB.mem_result : WB.alu_result;
        if (WB.r_out[0] != 0) {
            SET_GPR(WB.r_out[0], result);
        }
    }
    
    /* Second half of cycle - Register read availability */
    if (WB.valid) {
        if (WB.r_out[0] != 0) set_reg_ready(WB.r_out[0], 1);
        if (WB.r_out[1] != 0) set_reg_ready(WB.r_out[1], 1);
    }
}
```

5. **Performance Monitoring**
```c
/* Statistics tracking */
static counter_t sim_cycle = 0;        // Cycle counter
static counter_t sim_num_insn = 0;     // Instruction counter
static counter_t sim_num_stalls = 0;   // Stall counter
static counter_t sim_num_refs = 0;     // Memory reference counter

/* IPC calculation */
stat_reg_formula(sdb, "sim_IPC",
                "instructions per cycle",
                "sim_num_insn / sim_num_cycles", NULL);
```

6. **RAW Benchmark Support**
```c
/* RAW dependency testing */
static int run_raw_benchmark = 0;
static int raw_benchmark_instrs_completed = 0;
static int raw_benchmark_size = 20;

/* Special handling in execute stage */
case ADDIU:
    if (run_raw_benchmark && ID_EX.inst_num < raw_benchmark_size) {
        if (ID_EX.inst_num == 0) {
            alu_result = 1;  // First instruction
        } else {
            alu_result = ID_EX.r_in_val[0] + 1;  // Subsequent instructions
        }
    }
    break;
```

These changes transform sim-safe from a functional simulator into a pipeline simulator that can:
- Execute multiple instructions simultaneously
- Handle data hazards through forwarding
- Provide detailed performance metrics
- Model pipeline behavior

The implementation maintains compatibility with sim-safe's program execution while adding the ability to analyze pipeline performance and behavior.

## 🏁 Compile and Run
1. Clone the repository using:
    ```bash
    git clone https://github.com/ShoubraTeam/understanding-how-pipelines-function.git
    ```
2. Build the project using:
   ```bash
   make
   ```
3. Run the simulation without forwarding:
   ```bash
   ./sim-pipe2 -v -max:inst 10 -forward false tests-pisa/bin.little/test-math
   ```
4. Run the simulation with forwarding:
   ```bash
   ./sim-pipe2 -v -max:inst 10 -forward true tests-pisa/bin.little/test-math
   ```

## 🧪 Testing and Validation
Our testing includes:
1. Running standard benchmark programs (tests-pisa/bin.little/test-math)
2. Comparing instruction issuing with and without forwarding
3. Performance analysis under different scenarios (with & without forwarding)

## 🧩 Challenges Encountered
1. **Configuration Issues**
   - Initial setup challenges with SimpleScalar
   - Compatibility issues with different benchmark programs (for example, invalid inst)

2. **Performance Analysis**
   - Difficulty in collecting accurate performance metrics
   - Challenges in displaying simulation results in an organized way

3. **Pipeline Hazards**
   - Complex scenarios involving data hazards (Read After Write — RAW)

## 🛠️ Handling Challenges

1. **Configuration Solutions**
   - Added more configuration to support forwarding and without forwarding
   ```c
   /* Configuration options in sim-pipe2 */
   void sim_reg_options(struct opt_odb_t *odb) {
       /* Forwarding option */
       opt_reg_flag(odb, "-forward", 
                   "enable data forwarding",
                   &forward, /* default */ TRUE,
                   /* print */ TRUE, /* format */ NULL);
       
       /* Instruction limit for testing */
       opt_reg_uint(odb, "-max:inst", 
                   "maximum number of inst's to execute",
                   &max_insts, /* default */ 0,
                   /* print */ TRUE, /* format */ NULL);
   }
   ```

2. **Performance Analysis**
   - Created a more organized format to display the different instructions issuing & cycles they occur in
   ```c
   /* Pipeline state display function */
   static void print_pipeline_state(void) {
       printf("\nCycle %llu:\n", sim_cycle);
       
       /* Print each stage's state */
       for (int stage = 0; stage < PIPE_STAGES; stage++) {
           printf("%s: ", stage_names[stage]);
           if (pipe_latch[stage].valid) {
               printf("PC=0x%08x, Inst=%d", 
                      pipe_latch[stage].pc,
                      pipe_latch[stage].inst_num);
               
               /* Show forwarding if enabled */
               if (forward && stage == D_STAGE) {
                   printf(" [Forwarding: %s]", 
                          pipe_latch[stage].stall ? "Stalled" : "Active");
               }
           } else {
               printf("Empty");
           }
           printf("\n");
       }
   }
   ```

3. **Pipeline Hazard Resolution**
   - Implemented forwarding paths vs without forwarding to see the difference between the two
   ```c
   /* Hazard detection and forwarding implementation */
   static void handle_data_hazards(void) {
       if (!forward) {
           /* Without forwarding - stall pipeline */
           if (detect_raw_hazard()) {
               ID_EX.stall = TRUE;
               sim_num_stalls++;
           }
       } else {
           /* With forwarding - implement forwarding paths */
           word_t forwarded_val;
           int found = 0;
           
           /* Try to get forwarded value */
           forwarded_val = get_forwarded_value(current_reg, &found);
           
           if (found) {
               /* Use forwarded value instead of stalling */
               current_val = forwarded_val;
           } else if (detect_raw_hazard()) {
               /* If no forwarding possible, stall */
               ID_EX.stall = TRUE;
               sim_num_stalls++;
           }
       }
   }
   
   /* RAW hazard detection */
   static int detect_raw_hazard(void) {
       /* Check if source register matches destination of instructions
          in EX or MEM stages */
       for (int i = 0; i < 3; i++) {
           if (r_in[i] != 0) {
               if ((EX_MEM.valid && EX_MEM.r_out[0] == r_in[i]) ||
                   (MEM_WB.valid && MEM_WB.r_out[0] == r_in[i])) {
                   return TRUE;
               }
           }
       }
       return FALSE;
   }
   ```

## 📝 Conclusion
Through this project, we gained valuable insights into:
- Pipeline architecture and its complexities
- The importance of forwarding in modern processors
- Performance optimization techniques
- Practical implementation of computer architecture concepts

## 📌 References
1. [SimpleScalar Documentation](https://github.com/toddmaustin/simplesim-3.0)
2. Computer Architecture: A Quantitative Approach (Hennessy & Patterson)

## 👥 Contributors
- Karim Wael ([@devkarim](https://github.com/devkarim))
- Fady Youssef ([@FADYYOUSEFF](https://github.com/FADYYOUSEFF))
- Abdelrahman Osman ([@abdalrahman-osman](https://github.com/abdalrahman-osman))
- Hanin Mustafa ([@HaninMustafa9](https://github.com/HaninMustafa9))
- Sara Alaa ([@Saraalaaali](https://github.com/Saraalaaali))

