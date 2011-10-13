#define __STDC_FORMAT_MACROS
#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

//this will be provided in Makefile macro
//#define LSTALL 2
void logInst(FILE* outputFile,
             int cycle, int micro, 
             int src1, int rdy1,
             int src2, int rdy2,
             int dest, char load, 
             uint32_t sb[]);

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
  
  uint32_t scoreboard[60];
  uint32_t cycle = 0;
  uint32_t latency = 0;

  memset(scoreboard, 0, 50*sizeof(uint32_t));

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
    //calculate latency of current instruction
    if (loadStore == 'L')
    {
        latency = LSTALL;//default value is 2
    }
    else 
    {
        latency = 1;//others are 1
    }

    while ((sourceRegister1 != -1 && 
            scoreboard[sourceRegister1] != 0) ||
           (sourceRegister2 != -1 &&
            scoreboard[sourceRegister2] != 0) ||
           //preserve dest register write order
           (destinationRegister != -1 &&
            scoreboard[destinationRegister] >= latency)) 
    {

#ifdef DEBUG
        int rdy1 = (scoreboard[sourceRegister1]==0 || 
                    sourceRegister1 == -1)?1:0;
        int rdy2 = (scoreboard[sourceRegister2]==0 || 
                    sourceRegister2 == -1)?1:0;
        //log the instruction
        logInst(outputFile, cycle, totalMicroops, sourceRegister1, rdy1,
                sourceRegister2, rdy2,
                destinationRegister, loadStore, scoreboard);
#endif
        cycle++;

    //decrease the cycle of pending registers 
        for (int i = 0; i < 50; ++i)
        {
            if (scoreboard[i] > 0)
                scoreboard[i]--;
        }
    }

#ifdef DEBUG
    int rdy1 = (scoreboard[sourceRegister1]==0 || 
                sourceRegister1 == -1)?1:0;
    int rdy2 = (scoreboard[sourceRegister2]==0 || 
                sourceRegister2 == -1)?1:0;
#endif

    if (loadStore == 'L')
    {
        scoreboard[destinationRegister] = LSTALL;//default value is 2
    }
    else if (destinationRegister != -1)
        scoreboard[destinationRegister] = 1;
    //log the instruction
#ifdef DEBUG
    logInst(outputFile, cycle, totalMicroops, sourceRegister1, rdy1, 
            sourceRegister2, rdy2,
            destinationRegister, loadStore, scoreboard);
#endif
    cycle++;
    //decrease the cycle of pending registers 
    for (int i = 0; i < 50; ++i)
    {
        if (scoreboard[i] > 0)
            scoreboard[i]--;
    }
  }
  
  fprintf(outputFile, "Processed %" PRIi64 " trace records.\n", totalMicroops);

  fprintf(outputFile, "Micro-ops: %" PRIi64 "\n", totalMicroops);
  fprintf(outputFile, "Macro-ops: %" PRIi64 "\n", totalMacroops);

  fprintf(outputFile, "TotalCycles: %4d\n", cycle);
}

void logInst(FILE* outputFile,
             int cycle, int micro, 
             int src1, int rdy1,
             int src2, int rdy2,
             int dest, char load, 
             uint32_t sb[])
{
    const char rdy[]="(Ready)";
    const char nrdy[]="(NotReady)";

    fprintf(outputFile, "%5d%6d %2d %-11s%2d %-11s -> %2d  %c  ",
                     cycle, micro, src1, (rdy1?rdy:nrdy),
                     src2, (rdy2?rdy:nrdy), dest, load);

    for (int i = 0; i < 50; ++i)
    {
        if (sb[i] == 0)
            fprintf(outputFile, "%c", '-');
        else
            fprintf(outputFile, "%d", sb[i]);
    }

    fprintf(outputFile, "\n");
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
