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

#define ADDRLEN 64

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
  int64_t totalMemAccess = 0;

  int logSmax = 22;
  int logSmin = 8;
  int logB    = 6;


  uint64_t **Q4cache;
  int64_t *Q4miss;

  Q4cache = (uint64_t **)malloc((logSmax - logSmin + 1) * sizeof(uint64_t*));
  for (int i = logSmin; i <= logSmax; ++i)
  {
      int S = (int)(pow(2, i)/pow(2,logB));

      Q4cache[i-logSmin] = (uint64_t *)malloc(S*sizeof(uint64_t));
      memset(Q4cache[i-logSmin], 0, S*sizeof(uint64_t));
  }
  Q4miss = (int64_t *)malloc((logSmax - logSmin + 1)*sizeof(int64_t));
  memset(Q4miss, 0, (logSmax - logSmin + 1)*sizeof(int64_t));

  
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
    //only deal with load and store
    if (loadStore == 'L' || loadStore == 'R')
    {
        totalMemAccess++;
    //Question 4: direct-mapped cache, block size 64 bytes, cache size
    //from 256 bytes (2^8) to 4MB (2^22)
        int lowerI = logSmin - logB, upperI = logSmax - logB; 
        for (int i = lowerI; i <= upperI; ++i)
        {
            int logT = ADDRLEN - i - logB;//number of tag bits
            uint64_t tag;
            int index;

            tag = (addressForMemoryOp>>(i+logB)) & ((1<<logT) - 1);
            index = (addressForMemoryOp>>logB) & ((1<<i) - 1);

            if (Q4cache[i - lowerI][index] != tag)
            {
                Q4miss[i - lowerI]++;
                Q4cache[i - lowerI][index] = tag;
            }
        }

    }
  }
  

  for (int i = logSmin; i <= logSmax; ++i)
  {
      free(Q4cache[i-logSmin]);
  }
  free(Q4cache);
  free(Q4miss);

  fprintf(outputFile, "Processed %" PRIi64 " trace records.\n", totalMicroops);

  fprintf(outputFile, "Micro-ops: %" PRIi64 "\n", totalMicroops);
  fprintf(outputFile, "Macro-ops: %" PRIi64 "\n", totalMacroops);

  fprintf(outputFile, "Question 4:\nCache size (log)\tCache miss rate\n");
  for (int i = logSmin; i <= logSmax; ++i)
  {
      fprintf(outputFile, "%16d\t%15f\n", 
                          i, (double)Q4miss[i-logSmin]/totalMemAccess);
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
