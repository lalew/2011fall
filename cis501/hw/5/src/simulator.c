#define __STDC_FORMAT_MACROS
#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <queue>

using std::queue;

#define AREG_SIZE 50
#define PREG_SIZE 256
#define RFLAG     49


typedef struct{
    int32_t arch_src1;
    int32_t arch_src2;
    int32_t arch_dest;
    int32_t phys_src1;
    int32_t phys_src2;
    int32_t phys_dest;
    int32_t phys_dest_ex;
    int32_t flag_phys;
    int32_t flag_phys_ex;
    int writeFlag;
    int readFlag;
    int src1_ready;
    int src2_ready;
    int dest_ready;
    int flag_ready;
    uint64_t mem_op_addr;
    char macroOp[12];
    char microOp[23];
    int issued;//0--not issued, 1--issued
    int fetch_cycle;
    int issue_cycle;
    int done_cycle;
    int commit_cycle;
} Microop;

void reg_rename(Microop &pInstr,
                uint32_t *maptable,
                queue<uint32_t> &freelist)
{
    if (pInstr.arch_src1 != -1)
        pInstr.phys_src1 = maptable[pInstr.arch_src1];
    if (pInstr.arch_src2 != -1)
        pInstr.phys_src2 = maptable[pInstr.arch_src2];
    if (pInstr.arch_dest != -1)
        pInstr.phys_dest_ex = maptable[pInstr.arch_dest];

    pInstr.flag_phys_ex = maptable[RFLAG];

    //new mapping
    if (pInstr.arch_dest != -1)
    {
        int32_t new_reg = freelist.front();
        freelist.pop();
        maptable[pInstr.arch_dest] = new_reg;
        pInstr.phys_dest = new_reg;
    }

    if (pInstr.writeFlag == 1)//write flag, need new register for r49
    {
        int32_t new_reg = freelist.front();
        freelist.pop();
        pInstr.flag_phys_ex = maptable[RFLAG];
        pInstr.flag_phys = new_reg; 
        maptable[RFLAG] = new_reg;
    }
    if (pInstr.readFlag == 1)
    {
        pInstr.flag_phys_ex = maptable[RFLAG];
    }

}

void log_info(const Microop &pInstr, 
              const int64_t &totalMicroops, 
              FILE* outputFile)
{
    fprintf(outputFile, "%lld", totalMicroops);
    if (pInstr.arch_src1 != -1)
        fprintf(outputFile, ", r%d -> p%d", 
                pInstr.arch_src1, pInstr.phys_src1);
    if (pInstr.arch_src2 != -1)
        fprintf(outputFile, ", r%d -> p%d", 
                pInstr.arch_src2, pInstr.phys_src2);
    if (pInstr.readFlag == 1)
        fprintf(outputFile, ", r%d -> p%d", 
                RFLAG, pInstr.flag_phys_ex);
    if (pInstr.arch_dest != -1)
        fprintf(outputFile, ", r%d -> p%d [p%d]", 
                pInstr.arch_dest, pInstr.phys_dest, pInstr.phys_dest_ex);
    if (pInstr.writeFlag == 1)
        fprintf(outputFile, ", r%d -> p%d [p%d]", 
                RFLAG, pInstr.flag_phys, pInstr.flag_phys_ex);
    fprintf(outputFile, " | %s %s\n",
            pInstr.macroOp, pInstr.microOp);
}


void simulate(FILE* inputFile, FILE* outputFile)
{
  // See the documentation to understand what these variables mean.
  int32_t microOpCount;
  uint64_t instructionAddress;
  int32_t sourceRegister1;
  int32_t sourceRegister2;
  int32_t destinationRegister;
  char conditionRegister;
  char TNnotBranch;
  char loadStore;
  int64_t immediate;
  uint64_t addressForMemoryOp;
  uint64_t fallthroughPC;
  uint64_t targetAddressTakenBranch;
  char macroOperation[12];
  char microOperation[23];

  int64_t totalMicroops = 0;
  int64_t totalMacroops = 0;

  int64_t totalCycle = 0;
  
  uint32_t maptable[AREG_SIZE];//architecture register from 0 to AREG_SIZE-1
  queue<uint32_t> freelist;//free list from AREG_SIZE to PREG_SIZE-1

  for (int i = 0; i < AREG_SIZE; ++i)
      maptable[i] = i;

  for (int i = AREG_SIZE; i < PREG_SIZE; ++i)
      freelist.push(i);

  fprintf(outputFile, "Processing trace...\n");
  
  while (true) {
    int result = fscanf(inputFile, 
                        "%" SCNi32
                        "%" SCNx64 
                        "%" SCNi32
                        "%" SCNi32
                        "%" SCNi32
                        " %c"
                        " %c"
                        " %c"
                        "%" SCNi64
                        "%" SCNx64
                        "%" SCNx64
                        "%" SCNx64
                        "%11s"
                        "%22s",
                        &microOpCount,
                        &instructionAddress,
                        &sourceRegister1,
                        &sourceRegister2,
                        &destinationRegister,
                        &conditionRegister,
                        &TNnotBranch,
                        &loadStore,
                        &immediate,
                        &addressForMemoryOp,
                        &fallthroughPC,
                        &targetAddressTakenBranch,
                        macroOperation,
                        microOperation);
                        
    if (result == EOF) {
      break;
    }

    if (result != 14) {
      fprintf(stderr, "Error parsing trace at line %" PRIi64 "\n", totalMicroops);
      abort();
    }

    // For each micro-op
    totalMicroops++;

    // For each macro-op:
    if (microOpCount == 1) {
      totalMacroops++;
    }

    //additional work
    Microop aInstr;
    aInstr.writeFlag = 0;//whether flag will be written or not
    aInstr.readFlag = 0;//flag is read or not
    strncpy(aInstr.microOp, microOperation, 23);
    strncpy(aInstr.macroOp, macroOperation, 12);

    aInstr.arch_src1 = sourceRegister1;
    aInstr.arch_src2 = sourceRegister2;
    aInstr.arch_dest = destinationRegister;
    if (conditionRegister == 'W')//write flag, need new register for r49
    {
        aInstr.writeFlag = 1;
    }
    if (conditionRegister == 'R')
    {
        aInstr.readFlag = 1;
    }

    //renaming
    reg_rename(aInstr, maptable, freelist);

    //debug info
    log_info(aInstr, totalMicroops, outputFile);
    
    //commit
    if (aInstr.arch_dest != -1)
        freelist.push(aInstr.phys_dest_ex);
    if (aInstr.writeFlag == 1)
        freelist.push(aInstr.flag_phys_ex);
    totalCycle++;
    
  }
  
  fprintf(outputFile, "Processed %" PRIi64 " trace records.\n", totalMicroops);

  fprintf(outputFile, "Micro-ops: %" PRIi64 "\n", totalMicroops);
  fprintf(outputFile, "Macro-ops: %" PRIi64 "\n", totalMacroops);

  fprintf(outputFile, "IPC is :%f\n", (double)totalMicroops/totalCycle);
}

int main(int argc, char *argv[]) 
{
  FILE *inputFile = stdin;
  FILE *outputFile = stdout;
  
  if (argc >= 2) {
    inputFile = fopen(argv[1], "r");
    assert(inputFile != NULL);
  }
  if (argc >= 3) {
    outputFile = fopen(argv[2], "w");
    assert(outputFile != NULL);
  }
  
  simulate(inputFile, outputFile);
  return 0;
}
