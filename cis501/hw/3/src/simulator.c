#define __STDC_FORMAT_MACROS
#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>


int16_t satureOp(int16_t counter, int16_t change)
{
    int16_t res = counter + change;
    if (res < 0)
        return 0;
    if (res > 3)
        return 3;
    return res;
}

uint32_t lastNbit(uint64_t pc, int nBit)
{
    uint64_t mask;
    
    mask = (1<<nBit) - 1;

    return (uint32_t)(pc & mask);
}

void logFile(FILE *outputFile, 
             int8_t *counter,
             int len,
             uint64_t pc,
             char branch, 
             int8_t predict,
             int64_t miss)
{
    char res[15] ={0};

    for (int i = 0; i < len; ++i)
    {
        char pred;

        switch (counter[i])
        {
            case 0:
                pred = 'N';
                break;
            case 1:
                pred = 'n';
                break;
            case 2:
                pred = 't';
                break;
            case 3:
                pred = 'T';
                break;
        }

        fprintf(outputFile, "%c", pred);
    }

    if ((branch == 'T' && predict >= 2) ||
        (branch == 'N' && predict <= 1))
    {
        strcpy(res, "correct");
    }
    else
    {
        strcpy(res, "incorrect");
    }
    char pred;

    if (predict <= 1)
        pred = 'N';

    if (predict >= 2)
        pred = 'T';

    fprintf(outputFile, "\t\t%x\t%c\t%c\t%-9s   %d\n",
                        pc, branch, pred, res, miss);
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
  
  //additional work
  int64_t totalConBranch = 0;
    
  int64_t staticT = 0;
  int64_t bMiss[19] = {0};//bimodal predictor miss rates
  int64_t gMiss[19] = {0};//gshare predictor miss rates
  int64_t tMiss[19] = {0};//tournament predictor miss rates

  int8_t **biCounter;   //0--strongly nt, 3--strongly t

  biCounter = (int8_t **)malloc(19*sizeof(int8_t*));
  for (int i = 2; i <= 20; ++i)
  {
      biCounter[i-2] = (int8_t *)malloc((1<<i)*sizeof(int8_t));
      memset(biCounter[i-2], 0, (1<<i)*sizeof(int8_t));
  }

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

    int8_t pred[1<<20];
    //only conditional branches will be processed   
    if (conditionRegister == 'R' && TNnotBranch != '-')
    {
        totalConBranch++;
        //Question 1: static branch predictor
        if (TNnotBranch == 'T')
            staticT++;

        //Question 1: bimodal predictor
        //from 2^2 to 2^20
        for (int i = 2; i <= 20; ++i)
        {
            int index = lastNbit(instructionAddress, i);


            memcpy(pred, biCounter[i-2], (1<<i)*sizeof(int8_t));
            //0, 1 not taken; 2, 3 taken
            //prediction wrong
            if (TNnotBranch == 'T')
            {
                if (biCounter[i-2][index] <= 1)
                    bMiss[i-2]++;

                biCounter[i-2][index] = satureOp(biCounter[i-2][index], 1);
            }
            if (TNnotBranch == 'N')
            {
                if (biCounter[i-2][index] >= 2)
                    bMiss[i-2]++;

                biCounter[i-2][index] = satureOp(biCounter[i-2][index],-1);
            }
        
            if (i == 3)
            {
                logFile(outputFile, pred, 1<<3, instructionAddress,
                        TNnotBranch, pred[index], bMiss[i-2]);
            }
            
        }
    }

  }

  for (int i = 2; i <= 20; ++i)
  {
      free(biCounter[i-2]);
  }
  free(biCounter);

  
  fprintf(outputFile, "Processed %" PRIi64 " trace records.\n", totalMicroops);

  fprintf(outputFile, "Micro-ops: %" PRIi64 "\n", totalMicroops);
  fprintf(outputFile, "Macro-ops: %" PRIi64 "\n", totalMacroops);

  fprintf(outputFile, "Total conditional branches: %d\n", totalConBranch);
  fprintf(outputFile, "Static branch predictor miss rate: "
                      "Alwasy taken: %f, always not taken %f\n",
                      (double)staticT/totalConBranch,
                      1-(double)staticT/totalConBranch);

  fprintf(outputFile, "Bimodal predictor miss rates:\n"
                      "hisotry length    miss rate\n");
  for (int i = 0; i < 19; ++i)
  {
      fprintf(outputFile, "%-14d    %9f\n", 
                          i+2, (double)bMiss[i]/totalConBranch);
  }
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
