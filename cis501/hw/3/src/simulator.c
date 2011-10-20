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

uint32_t xorNBit(uint32_t addr, uint8_t his, int aLen, int hLen)
{
   int aMask;
   int hMask;
   uint32_t xorHis;

   aMask = ((1<<aLen) - 1) << hLen;
   hMask = (1<<hLen) - 1;

   xorHis = (addr & hMask) ^ his;

   addr = (addr & aMask) | xorHis;

   return addr;
}

uint16_t histShift(uint16_t old, char ifTake, int len)
{
    uint16_t mask;
    mask = (1<<len) - 1;

    old = old << 1;

    if (ifTake == 'T')//taken
    {
        old = old | 1;
    }
    else if (ifTake == 'N')//not taken
    {
        old = old & (mask - 1);
    }
    old = old & mask;

    return old;
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

    fprintf(outputFile, "\t\t%llx\t%c\t%c\t%-9s   %lld\n",
                        pc, branch, pred, res, miss);
}

void bimodal(int8_t **BiCounter, int64_t *bMiss, int cntBits, 
             uint64_t instructionAddress, char TNnotBranch)
{
    int index = lastNbit(instructionAddress, cntBits);
    int8_t pred[1<<20];

    memcpy(pred, BiCounter[cntBits-2], (1<<cntBits)*sizeof(int8_t));
    //0, 1 not taken; 2, 3 taken
    //prediction wrong
    if (TNnotBranch == 'T')
    {
        if (BiCounter[cntBits-2][index] <= 1)
            bMiss[cntBits-2]++;

        BiCounter[cntBits-2][index] = 
            satureOp(BiCounter[cntBits-2][index], 1);
    }
    if (TNnotBranch == 'N')
    {
         if (BiCounter[cntBits-2][index] >= 2)
             bMiss[cntBits-2]++;

         BiCounter[cntBits-2][index] = 
             satureOp(BiCounter[cntBits-2][index],-1);
    }
        
#ifdef DEBUG
    logFile(outputFile, pred, 1<<3, instructionAddress,
                    TNnotBranch, pred[index], bMiss[cntBits-2]);
#endif

}

void gshare(int8_t **GsCounter, int64_t *gMiss, uint32_t history, int cntBits,
            int hisLen, uint64_t instructionAddress, char TNnotBranch)
{
    int index = lastNbit(instructionAddress, cntBits);
    int8_t pred[1<<20];

    index = xorNBit(index, history, cntBits, hisLen);

    memcpy(pred, GsCounter[cntBits-2], (1<<cntBits)*sizeof(int8_t));
    //0, 1 not taken; 2, 3 taken
    //prediction wrong
    if (TNnotBranch == 'T')
    {
        if (GsCounter[cntBits-2][index] <= 1)
            gMiss[cntBits-2]++;

        GsCounter[cntBits-2][index] = satureOp(GsCounter[cntBits-2][index], 1);
    }
    if (TNnotBranch == 'N')
    {
        if (GsCounter[cntBits-2][index] >= 2)
            gMiss[cntBits-2]++;

        GsCounter[cntBits-2][index] = satureOp(GsCounter[cntBits-2][index],-1);
     }

#ifdef DEBUG
     logFile(outputFile, pred, 1<<4, instructionAddress,
                    TNnotBranch, pred[index], gMiss[cntBits-2]);
#endif
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
  uint8_t history = 0;
    
  int64_t staticT = 0;
  int64_t bMiss[19] = {0};//bimodal predictor miss rates
  int64_t gMiss[19] = {0};//gshare predictor miss rates
  int64_t tMiss[19] = {0};//tournament predictor miss rates

  int8_t **BiCounter;   //0--strongly nt, 3--strongly t
  int8_t **GsCounter;   //0--strongly nt, 3--strongly t
  int8_t **TuCounter;   //0--strongly bimodal, 3--strongly gshare

  BiCounter = (int8_t **)malloc(19*sizeof(int8_t*));
  for (int i = 2; i <= 20; ++i)
  {
      BiCounter[i-2] = (int8_t *)malloc((1<<i)*sizeof(int8_t));
      memset(BiCounter[i-2], 0, (1<<i)*sizeof(int8_t));
  }

  GsCounter = (int8_t **)malloc(19*sizeof(int8_t*));
  for (int i = 2; i <= 20; ++i)
  {
      GsCounter[i-2] = (int8_t *)malloc((1<<i)*sizeof(int8_t));
      memset(GsCounter[i-2], 0, (1<<i)*sizeof(int8_t));
  }

  TuCounter = (int8_t **)malloc(19*sizeof(int8_t*));
  for (int i = 2; i <= 20; ++i)
  {
      TuCounter[i-2] = (int8_t *)malloc((1<<i)*sizeof(int8_t));
      memset(TuCounter[i-2], 0, (1<<i)*sizeof(int8_t));
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

        //Question 2: bimodal predictor
        //from 2^2 to 2^20
        for (int i = 2; i <= 20; ++i)
        {
            bimodal(BiCounter, bMiss, i, instructionAddress, TNnotBranch);
        }

        //Question 3: Gshare predictor
        int hisLen = 8;
        for (int i = 2; i <= 20; ++i)
        {
            gshare(GsCounter, gMiss, history, i, hisLen,
                   instructionAddress, TNnotBranch);
        }
        history = histShift(history, TNnotBranch, hisLen);

        //Question 4: Gshare history length
        
        //Question 5: Gshare history length is equal to predictor size

        //Question 5: Tournament predictor

    }

  }

  for (int i = 2; i <= 20; ++i)
  {
      free(BiCounter[i-2]);
      free(GsCounter[i-2]);
      free(TuCounter[i-2]);
  }
  free(BiCounter);
  free(GsCounter);
  free(TuCounter);

  
  fprintf(outputFile, "Processed %" PRIi64 " trace records.\n", totalMicroops);

  fprintf(outputFile, "Micro-ops: %" PRIi64 "\n", totalMicroops);
  fprintf(outputFile, "Macro-ops: %" PRIi64 "\n", totalMacroops);

  fprintf(outputFile, "Total conditional branches: %lld\n", totalConBranch);
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

  fprintf(outputFile, "Gshare predictor miss rates:\n"
                      "hisotry length    miss rate\n");
  for (int i = 0; i < 19; ++i)
  {
      fprintf(outputFile, "%-14d    %9f\n", 
                          i+2, (double)gMiss[i]/totalConBranch);
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
