#define __STDC_FORMAT_MACROS
#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <math.h>

#define LOADS   0
#define STORES  1
#define UBRANCH 2
#define CBRANCH 3
#define OTHER   4

const char instMix[][25]=
{"Loads", "Stores",
 "Unconditional branches",
 "Conditional branches",
 "Other"
};

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
  
  //additional work
  int opsDistr[100];//assume the max number of micro per macro is 100
  int maxMicro = 0;
  double avgMicro = 0;

  int sizeDistr[100];
  int totalSize = 0;
  double avgSize = 0;

  int branchDistr[100];
  int totalBranch = 0;

  int mixDistr[5];

  int fusionNum = 0;
  char preFlag = ' ';

  memset(opsDistr, 0, sizeof(int)*100);
  memset(sizeDistr, 0, sizeof(int)*100);
  memset(branchDistr, 0, sizeof(int)*100);
  memset(mixDistr, 0, sizeof(int)*5);
  
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
      //for the last instruction
      opsDistr[maxMicro]++;
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

      //additional work
      opsDistr[maxMicro]++;
      
      int instSize = fallthroughPC - instructionAddress;
      sizeDistr[instSize] ++;
      totalSize += instSize;
    }

    //additional works
    //record the number of micro per macro
    maxMicro = microOpCount;
    
    //distribution of branch distances
    if (targetAddressTakenBranch != 0)
    {
        int numBits = 2 
                      + floor(log2(abs(instructionAddress 
                      - targetAddressTakenBranch)));
        branchDistr[numBits]++;
        totalBranch++;
    }

    //Instruction Mix
    if (loadStore == 'L')
        mixDistr[LOADS]++;
    else if (loadStore == 'S')
        mixDistr[STORES]++;
    else if (targetAddressTakenBranch != 0 
             && conditionRegister == '-')
        mixDistr[UBRANCH]++;
    else if (targetAddressTakenBranch != 0
             && conditionRegister == 'R')
        mixDistr[CBRANCH]++;
    else
        mixDistr[OTHER]++;

    //operation fusion
    if (preFlag == 'W' 
        && targetAddressTakenBranch != 0
        && conditionRegister == 'R')
    {
        fusionNum++;
    }

    preFlag = conditionRegister;

  }
  
  fprintf(outputFile, "Processed %" PRIi64 " trace records.\n", totalMicroops);

  fprintf(outputFile, "Micro-ops: %" PRIi64 "\n", totalMicroops);
  fprintf(outputFile, "Macro-ops: %" PRIi64 "\n", totalMacroops);
  
  //Question 1
  fprintf(outputFile, "Average number of micro-ops per macro-op: %1.2f\n", 
                       (double)totalMicroops/totalMacroops);
  fprintf(outputFile, "Micro per Macro\t\tPercentage\n");
  for (int i = 1; i < 100; ++i)
  {
      if (opsDistr[i] != 0)
      {
          double distr = (double)opsDistr[i]/totalMacroops*100;
          fprintf(outputFile, "%15d\t\t%.2f\n", i, distr);
          avgMicro += distr*i/100;
      }
  }
  fprintf(outputFile, "Average number of micro-ops per macro-op: %1.2f\n\n",
                       avgMicro);

  //Question 2
  fprintf(outputFile, "Average bytes per Macro-op: %1.2f\n", 
                      (double)totalSize/totalMacroops);

  fprintf(outputFile, "Bytes per Macro\t\tPercentage\n");
  for (int i = 1; i < 100; ++i)
  {
      if (sizeDistr[i] != 0)
      {
          double distr = (double)sizeDistr[i]/totalMacroops*100;
          fprintf(outputFile, "%15d\t\t%.5f\n", i, distr);
          avgSize += distr*i/100;
      }
  }
  fprintf(outputFile, "Average bytes per Macro-ops:%1.2f\n\n", avgSize);

  //Question 3
  fprintf(outputFile, "Cumulative number of bits per Macro\t\tPercentage\n");
  double cumPer = 0;
  double cum8bit = 0, cum16bit = 0;
  for (int i = 1; i < 100; ++i)
  {
      if (branchDistr[i] != 0)
      {
          double distr = (double)branchDistr[i]/totalBranch*100;
          cumPer += distr;
          fprintf(outputFile, "%35d\t\t%.2f\n", i, cumPer);

          if (i == 8)
              cum8bit = cumPer;
          if (i == 16)
              cum16bit = cumPer;
      }
  }

  fprintf(outputFile, "%1.2f percent of branches can be encoded with 8 bits\n",
                       cum8bit);
  fprintf(outputFile, "%1.2f percent of branches can be encoded with 16 bits\n",
                       cum16bit);

  //Question 4
  fprintf(outputFile, "\n     Type of instructions\t\tPercentage\n");
  for (int i = 0; i < 5; ++i)
  {
      fprintf(outputFile, "%25s\t\t%.2f\n", instMix[i], 
              (double)mixDistr[i]/totalMicroops*100);
  }

  //Question 5
  fprintf(outputFile, "\nThe increase of total micro-ops for larger branch:"
          "%.2f%%",
          (100-cum8bit)*(mixDistr[UBRANCH]+mixDistr[CBRANCH])/totalMicroops);

  //Question 6
  fprintf(outputFile, "\n%d pairs (%.2f percent) of all micro-ops are "
          "eligible for such fusion.\n", 
          fusionNum, (double)fusionNum/totalMicroops*100*2);
  fprintf(outputFile, "It will be %.2f %% faster.\n", 
          (double)totalMicroops/(totalMicroops-fusionNum)*100 - 100);


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
