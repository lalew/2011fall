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

typedef struct{
    uint64_t tags[2];
    int dirty[2];//0--clean, 1--dirty
    int lru;
} twoWaySet;

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
  int wayNum  = 2;

  int Q7logSmax = 11;
  int Q7logSmin = 5;

  int Q8logS = 8;
  int Q8logB = 6;
  int Q8logT = ADDRLEN - Q8logS - Q8logB;
  int Q8Predmin = 0;//1 entry
  int Q8Predmax = 16;//2^16 entry
  int64_t Q8hit = 0;

  uint64_t **Q4cache;
  int64_t *Q4miss;

  twoWaySet **Q5cache;
  int64_t *Q5miss;

  int64_t *Q6WriteThr;
  int64_t *Q6WriteBck;

  twoWaySet **Q7cache;
  int64_t *Q7miss;
  int64_t *Q7WrtBck;

  twoWaySet *Q8cache;
  int **wayPred;
  int64_t *Q8misPred;
  int64_t Q8miss = 0;

  Q4cache = (uint64_t **)malloc((logSmax - logSmin + 1) * sizeof(uint64_t*));
  for (int i = logSmin; i <= logSmax; ++i)
  {
      int S = (int)(pow(2, i)/pow(2,logB));

      Q4cache[i-logSmin] = (uint64_t *)malloc(S*sizeof(uint64_t));
      memset(Q4cache[i-logSmin], 0, S*sizeof(uint64_t));
  }
  Q4miss = (int64_t *)malloc((logSmax - logSmin + 1)*sizeof(int64_t));
  memset(Q4miss, 0, (logSmax - logSmin + 1)*sizeof(int64_t));

  Q5cache = (twoWaySet **)malloc((logSmax - logSmin + 1)*sizeof(twoWaySet*));
  for (int i = logSmin; i <= logSmax; ++i)
  {
      int S = (int)(pow(2, i)/pow(2,logB)/wayNum);//number of sets
      
      Q5cache[i-logSmin] = (twoWaySet *)malloc(S*sizeof(twoWaySet));
      memset(Q5cache[i-logSmin], 0, S*sizeof(twoWaySet));
  }
  Q5miss = (int64_t *)malloc((logSmax - logSmin +1)*sizeof(int64_t));
  memset(Q5miss, 0, (logSmax - logSmin +1)*sizeof(int64_t));
  
  Q6WriteThr = (int64_t *)malloc((logSmax - logSmin +1)*sizeof(int64_t));
  memset(Q6WriteThr, 0, (logSmax - logSmin +1)*sizeof(int64_t));

  Q6WriteBck = (int64_t *)malloc((logSmax - logSmin +1)*sizeof(int64_t));
  memset(Q6WriteBck, 0, (logSmax - logSmin +1)*sizeof(int64_t));

  Q7cache = (twoWaySet **)malloc(7*sizeof(twoWaySet*));
  for (int i = Q7logSmax; i >= Q7logSmin; --i)
  {
      int S = (int)pow(2, i);
      Q7cache[Q7logSmax - i] = (twoWaySet *)malloc(S*sizeof(twoWaySet));
      memset(Q7cache[Q7logSmax - i], 0, S*sizeof(twoWaySet));
  }
  Q7miss = (int64_t *)malloc((Q7logSmax - Q7logSmin +1)*sizeof(int64_t));
  memset(Q7miss, 0, (Q7logSmax - Q7logSmin +1)*sizeof(int64_t));
  Q7WrtBck = (int64_t *)malloc((Q7logSmax - Q7logSmin +1)*sizeof(int64_t)); 
  memset(Q7WrtBck, 0, (Q7logSmax - Q7logSmin +1)*sizeof(int64_t));

  int S = (int)pow(2, Q8logS);//number of sets
  Q8cache = (twoWaySet *)malloc(S*sizeof(twoWaySet));

  wayPred = (int **)malloc((Q8Predmax - Q8Predmin + 1)*sizeof(int *));
  for (int i = Q8Predmin; i <= Q8Predmax; ++i)
  {
      int entryN = (int)pow(2, i);
      wayPred[i] = (int *)malloc(entryN*sizeof(int));
      memset(wayPred[i], 0, entryN*sizeof(int));
  }
  //mis-pred rate for each way predictor
  Q8misPred = (int64_t *)malloc((Q8Predmax - Q8Predmin + 1)*sizeof(int64_t));
  memset(Q8misPred, 0, (Q8Predmax - Q8Predmin + 1)*sizeof(int64_t));


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
    if (loadStore == 'L' || loadStore == 'S')
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

            tag = (addressForMemoryOp>>(i+logB)) & (((uint64_t)1<<logT) - 1);
            index = (addressForMemoryOp>>logB) & (((uint64_t)1<<i) - 1);

            if (Q4cache[i - lowerI][index] != tag)
            {
                Q4miss[i - lowerI]++;
                Q4cache[i - lowerI][index] = tag;
            }
        }
    //Question 5: two-way set associative, a single LRU bit
    //block size 64 bytes, cache size from 256 bytes (2^8) to
    //4MB (2^22)
        lowerI = logSmin - logB - 1, upperI = logSmax - logB - 1; 
        for (int i = lowerI; i <= upperI; ++i)
        {
            int logT = ADDRLEN - i - logB;//number of tag bits
            uint64_t tag;
            int index;

            tag = (addressForMemoryOp>>(i+logB)) & (((uint64_t)1<<logT) - 1);
            index = (addressForMemoryOp>>logB) & (((uint64_t)1<<i) - 1);
#ifdef DEBUG
            if (i == lowerI)
            {
                fprintf(outputFile, "[Set 0: {Way 0:%llx, %c} {Way 1:%llx, %c}"
                        " LRU: %d] [Set 1: {Way 0:%llx, %c} {Way 1:%llx, %c}"
                        " LRU: %d]  | %llx %c ", 
                        Q5cache[i - lowerI][0].tags[0],
                        Q5cache[i - lowerI][0].dirty[0]==0?'C':'D',
                        Q5cache[i - lowerI][0].tags[1],
                        Q5cache[i - lowerI][0].dirty[1]==0?'C':'D',
                        Q5cache[i - lowerI][0].lru,
                        Q5cache[i - lowerI][1].tags[0],
                        Q5cache[i - lowerI][1].dirty[0]==0?'C':'D',
                        Q5cache[i - lowerI][1].tags[1],
                        Q5cache[i - lowerI][1].dirty[1]==0?'C':'D',
                        Q5cache[i - lowerI][1].lru,
                        (addressForMemoryOp>>logB)<<logB,
                        loadStore
                        );
            }
#endif

            //Q5: read/write miss
            if (Q5cache[i - lowerI][index].tags[0] != tag &&
                Q5cache[i - lowerI][index].tags[1] != tag)
            {
                int lru = Q5cache[i - lowerI][index].lru;

                //debug
#ifdef DEBUG
                if (i == lowerI)
                {
                    fprintf(outputFile, "miss ");

                    if (Q5cache[i - lowerI][index].dirty[lru] == 1)
                    {
                        fprintf(outputFile, " dirty\n");
                    }
                    else//clean
                    {
                        fprintf(outputFile, " clean\n");
                    }
                }
#endif
                //Q5
                Q5miss[i - lowerI]++;
                Q5cache[i - lowerI][index].tags[lru] = tag;

                //Q6: miss leads to evict LRU block
                //dirty
                if (Q5cache[i - lowerI][index].dirty[lru] == 1)
                {
                    Q6WriteBck[i - lowerI]++;
                    Q5cache[i - lowerI][index].dirty[lru] = 0;
                }

                //if (loadStore == 'S')
                //    Q5cache[i - lowerI][index].dirty[lru] = 1;
                //Q5
                Q5cache[i - lowerI][index].lru ^= 1;
                

            }
            else//hit
            {
#ifdef DEBUG
                if (i == lowerI)
                    fprintf(outputFile, " hit    --\n");
#endif

                if (Q5cache[i - lowerI][index].tags[0] == tag)
                {
                    Q5cache[i - lowerI][index].lru = 1; 
            
                    // if (loadStore == 'S')
                    //{
                    //    Q5cache[i - lowerI][index].dirty[0] = 1;
                    //}
                }
                else if (Q5cache[i - lowerI][index].tags[1] == tag)
                {
                    Q5cache[i - lowerI][index].lru = 0; 

                    //if (loadStore == 'S')
                    //{
                    //    Q5cache[i - lowerI][index].dirty[1] = 1;
                    //}
                }
            }


            //Q6: write hit/miss
            if (loadStore == 'S')
            {
                if (Q5cache[i - lowerI][index].tags[0] == tag)
                {
                    Q5cache[i - lowerI][index].dirty[0] = 1;
                    
                }
                else if (Q5cache[i - lowerI][index].tags[1] == tag)
                {
                    Q5cache[i - lowerI][index].dirty[1] = 1;
                }
            
                //Q6: write through, namely store times 
                Q6WriteThr[i - lowerI]++;
            }
        }

        //Q7: variant block size from 8B to 512B
        for (int i = Q7logSmax; i >= Q7logSmin; --i)//from 11 to 5
        {
            int Q7logT = ADDRLEN - 14;//32KB two way set assoc cache
            int Q7logB = 14 - i;//32KB uses 14bits for both set and block
            uint64_t tag;
            int index;

            tag = (addressForMemoryOp>>14) & (((uint64_t)1<<Q7logT) - 1);
            index = (addressForMemoryOp>>Q7logB) & (((uint64_t)1<<i) - 1);
            
            //miss
            if (Q7cache[Q7logSmax - i][index].tags[0] != tag &&
                Q7cache[Q7logSmax - i][index].tags[1] != tag)
            {
                int lru = Q7cache[Q7logSmax - i][index].lru;
                
                Q7miss[Q7logSmax - i]++;
                Q7cache[Q7logSmax - i][index].tags[lru] = tag;

                //miss leads to evict LRU block
                //dirty
                if (Q7cache[Q7logSmax - i][index].dirty[lru] == 1)
                {
                    Q7WrtBck[Q7logSmax - i]++;
                    Q7cache[Q7logSmax - i][index].dirty[lru] = 0;
                }
                
                Q7cache[Q7logSmax - i][index].lru ^= 1;

            }
            else//hit
            {
                if (Q7cache[Q7logSmax - i][index].tags[0] == tag)
                {
                    Q7cache[Q7logSmax - i][index].lru = 1; 
                }
                else if (Q7cache[Q7logSmax - i][index].tags[1] == tag)
                {
                    Q7cache[Q7logSmax - i][index].lru = 0; 
                }
            }
            
            if (loadStore == 'S')
            {
                if (Q7cache[Q7logSmax - i][index].tags[0] == tag)
                {
                    Q7cache[Q7logSmax - i][index].dirty[0] = 1;
                    
                }
                else if (Q7cache[Q7logSmax - i][index].tags[1] == tag)
                {
                    Q7cache[Q7logSmax - i][index].dirty[1] = 1;
                }
            
            }

        }

        //Q8
        {
            uint64_t tag;
            int index;

            tag = (addressForMemoryOp>>(Q8logS+Q8logB)) &
                  (((uint64_t)1<<Q8logT) - 1);
            index = (addressForMemoryOp>>Q8logB) & 
                  (((uint64_t)1<<Q8logS) - 1);
            
            //prediction
            for (int i = Q8Predmin; i <= Q8Predmax; ++i)
            {
                //int predN = (int)pow(2, i);//predictor size
                int indexP = (addressForMemoryOp>>Q8logB) &
                             ((1<<i) - 1);
                int wayP = wayPred[i][indexP];//way predicted

                //prediction does not match the tag, but the other
                //way matches, then misprediction.
                if (Q8cache[index].tags[wayP] != tag &&
                    Q8cache[index].tags[wayP^1] == tag)
                {
                    Q8misPred[i]++;
                    //update predictor
                    wayPred[i][indexP] = wayP^1;
                }
                //if cache miss, store data to LRU cache, update predictor
                if (Q8cache[index].tags[0] != tag &&
                    Q8cache[index].tags[1] != tag)
                {
                    wayPred[i][indexP] = Q8cache[index].lru;
                }

            }


            //miss
            if (Q8cache[index].tags[0] != tag &&
                Q8cache[index].tags[1] != tag)
            {
                int lru = Q8cache[index].lru;
                
                Q8miss++;

                Q8cache[index].tags[lru] = tag;

                //miss leads to evict LRU block
                //dirty
                if (Q8cache[index].dirty[lru] == 1)
                {
                    Q8cache[index].dirty[lru] = 0;
                }
                
                Q8cache[index].lru ^= 1;

            }
            else//hit
            {
                Q8hit++;
                if (Q8cache[index].tags[0] == tag)
                {
                    Q8cache[index].lru = 1; 
                }
                else if (Q8cache[index].tags[1] == tag)
                {
                    Q8cache[index].lru = 0; 
                }
            }
            
            if (loadStore == 'S')
            {
                if (Q8cache[index].tags[0] == tag)
                {
                    Q8cache[index].dirty[0] = 1;
                    
                }
                else if (Q8cache[index].tags[1] == tag)
                {
                    Q8cache[index].dirty[1] = 1;
                }
            
            }
        }//Q8

    }
  }
  

    fprintf(outputFile, "Processed %" PRIi64 " trace records.\n", totalMicroops);

  fprintf(outputFile, "Micro-ops: %" PRIi64 "\n", totalMicroops);
  fprintf(outputFile, "Macro-ops: %" PRIi64 "\n", totalMacroops);

  fprintf(outputFile, "Question 4:\nCache size (log)\tCache miss rate\t"
            "Miss number\n");
  for (int i = logSmin; i <= logSmax; ++i)
  {
      fprintf(outputFile, "%16d\t%15f%11lld\n", 
                          i, 
                          (double)Q4miss[i-logSmin]/totalMemAccess,
                          Q4miss[i-logSmin]);
  }
  
  fprintf(outputFile, "Question 5:\nCache size (log)\tCache miss rate\n");
  for (int i = logSmin; i <= logSmax; ++i)
  {
      fprintf(outputFile, "%16d\t%15f\n", 
                          i, (double)Q5miss[i-logSmin]/totalMemAccess);
  }

  fprintf(outputFile, "Question 6:\nCache size (log)\tTraffic Thr\t"
                      "Traffic Bck\n");
  for (int i = logSmin; i <= logSmax; ++i)
  {
      fprintf(outputFile, "%16d\t%11f\t%11f\n", 
              i, 
      (double)(Q6WriteThr[i-logSmin]*4+Q5miss[i-logSmin]*64)/totalMemAccess,
      (double)(Q6WriteBck[i-logSmin]*64+Q5miss[i-logSmin]*64)/totalMemAccess
      );
/*      fprintf(outputFile, "%16d\t%11lld\t%11lld\n",
              i,
              Q6WriteThr[i-logSmin],
              Q6WriteBck[i-logSmin]);*/
  }

  fprintf(outputFile, "Question 7:\nBlock size (log)\tMiss rate\tTraffic\n");
  for (int i = 3; i <= 9; ++i)//from 8B to 512B
  {
      int bSize = (int)pow(2,i);
      fprintf(outputFile, "%16d\t%9f\t%7f\n",
              i,
              (double)Q7miss[i-3]/totalMemAccess,
              (double)(Q7WrtBck[i-3]+Q7miss[i-3])*bSize/totalMemAccess);
  }

  fprintf(outputFile, "Question 8: cache miss: %lld\n"
          "Predictor Size (log)\tMis-prediction\tMis-pred+miss\n", Q8miss);
  for (int i = Q8Predmin; i <= Q8Predmax; ++i)
  {
      fprintf(outputFile, "%20d\t%14f\t%13lld\n",
              i,
              (double)Q8misPred[i]/Q8hit,
              Q8misPred[i]+Q8miss);
  }
  
  //clean up
  for (int i = logSmin; i <= logSmax; ++i)
  {
      free(Q4cache[i-logSmin]);
  }
  free(Q4cache);
  free(Q4miss);

  for (int i = logSmin; i <= logSmax; ++i)
  {
      free(Q5cache[i-logSmin]);
  }
  free(Q5cache);
  free(Q5miss);

  free(Q6WriteThr);
  free(Q6WriteBck);

  for (int i = Q7logSmax; i >= Q7logSmin; --i)
  {
      free(Q7cache[Q7logSmax - i]);
  }
  free(Q7cache);
  free(Q7miss);
  free(Q7WrtBck);


  free(Q8cache);
  for (int i = Q8Predmin; i <= Q8Predmax; ++i)
  {
      free(wayPred[i]);
  }
  free(wayPred);
  free(Q8misPred);
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
