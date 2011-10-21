#define __STDC_FORMAT_MACROS
#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>


inline int16_t satureOp(int16_t counter, int16_t change)
{
    int16_t res = counter + change;
    if (res < 0)
        return 0;
    if (res > 3)
        return 3;
    return res;
}

inline uint32_t lastNbit(uint64_t pc, int nBit)
{
    uint64_t mask;
    
    mask = (1<<nBit) - 1;

    return (uint32_t)(pc & mask);
}

inline uint32_t xorNBit(uint32_t addr, uint32_t his, int len)
{
   uint32_t mask;

   mask = (1<<len) - 1;

   addr = addr ^ (his & mask);

   return addr;
}

inline uint32_t histShift(uint32_t old, char ifTake, int len)
{
    uint32_t mask;
    mask = (1<<len) - 1;

    old = old << 1;

    if (ifTake == 'T')//taken
    {
        old = old | 1;
    }
    else if (ifTake == 'N')//not taken
    {
        old = old & ~1;
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
             int64_t miss,
             uint32_t his,
             int hLen,
             int8_t *bC,
             int8_t *gC)
{
    char res[15] ={0};

    for (int i = 0; i < len; ++i)
    {
        char pred = 'N';

        switch (counter[i])
        {
            case 0:
                pred = 'B';
                break;
            case 1:
                pred = 'b';
                break;
            case 2:
                pred = 'g';
                break;
            case 3:
                pred = 'G';
                break;
        }

        fprintf(outputFile, "%c", pred);
    }
    fprintf(outputFile, " ");
    //=====
    for (int i = 0; i < len; ++i)
    {
        char pred = 'N';

        switch (bC[i])
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
    fprintf(outputFile, " ");
    
    for (int i = 0; i < len*2; ++i)
    {
        char pred = 'N';

        switch (gC[i])
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
    fprintf(outputFile, " ");
    //=====
    for (int i = hLen - 1; i >= 0; --i)
    {
    	if (his & (1<<i))
    		fprintf(outputFile, "T");
    	else
    		fprintf(outputFile, "N");
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
  uint32_t history = 0;
  uint32_t historyQ4[2] = {0};
  uint32_t historyQ5[19] = {0};
  
  //uint32_t historyG = 0;
    
  int64_t staticT = 0;
  int64_t bMiss[21] = {0};//bimodal predictor miss rates
  int64_t gMiss[21] = {0};//gshare predictor miss rates
  int64_t gMissQ4[2][20] = {{0}};//gshare 0 to 19 history and 10 & 16 entry bit 
  int64_t gMissQ5[21] = {0};//gshare 2 to 20

  int64_t tMissQ6[21] = {0};//tournament predictor miss rates

  int64_t tMissQ7[21] = {0};

  int8_t **BiCounter;   //0--strongly nt, 3--strongly t
  
  int8_t **GsCounter;   //0--strongly nt, 3--strongly t
  
  int8_t ***GsCounterQ4;//0--strongly nt, 3--strongly t
  
  int8_t **GsCounterQ5; //0--strongly nt, 3--strongly t

  int8_t **ToCounterQ6;   //0--strongly bimodal, 3--strongly gshare
  
  int8_t **ToCounterQ7;
  
  //int8_t pred[1<<20];
  //int8_t predB[1<<20];
  //int8_t predG[1<<20];

  BiCounter = (int8_t **)malloc(21*sizeof(int8_t*));
  for (int i = 0; i <= 20; ++i)
  {
      BiCounter[i] = (int8_t *)malloc((1<<i)*sizeof(int8_t));
      memset(BiCounter[i], 0, (1<<i)*sizeof(int8_t));
  }

  GsCounter = (int8_t **)malloc(21*sizeof(int8_t*));
  for (int i = 0; i <= 20; ++i)
  {
      GsCounter[i] = (int8_t *)malloc((1<<i)*sizeof(int8_t));
      memset(GsCounter[i], 0, (1<<i)*sizeof(int8_t));
  }

  //for 2^10 and 2^16
  GsCounterQ4 = (int8_t ***)malloc(2*sizeof(int8_t**));
  for (int j = 10; j <= 16; j=j+6)
  {
  	int index;
    if (j == 10)
        index = 0;
    else if (j == 16)
        index = 1;
        
      GsCounterQ4[index] = (int8_t **)malloc(20*sizeof(int8_t*));
      for (int i = 0; i < 20; ++i)
      {
          

          GsCounterQ4[index][i] = (int8_t *)malloc((1<<j)*sizeof(int8_t));
          if (GsCounterQ4[index][i] == NULL)
          {
              printf("error!\n");
              exit(1);
          }
          memset(GsCounterQ4[index][i], 0, (1<<j)*sizeof(int8_t));
      }
  }

  GsCounterQ5 = (int8_t **)malloc(21*sizeof(int8_t*));
  for (int i = 0; i <= 20; ++i)
  {
      GsCounterQ5[i] = (int8_t *)malloc((1<<i)*sizeof(int8_t));
      memset(GsCounterQ5[i], 0, (1<<i)*sizeof(int8_t));
  }

  ToCounterQ6 = (int8_t **)malloc(21*sizeof(int8_t*));
  for (int i = 0; i <= 20; ++i)
  {
      ToCounterQ6[i] = (int8_t *)malloc((1<<i)*sizeof(int8_t));
      memset(ToCounterQ6[i], 0, (1<<i)*sizeof(int8_t));
  }
  
  ToCounterQ7 = (int8_t **)malloc(21*sizeof(int8_t*));
  for (int i = 0; i <= 20; ++i)
  {
      ToCounterQ7[i] = (int8_t *)malloc((1<<i)*sizeof(int8_t));
      memset(ToCounterQ7[i], 0, (1<<i)*sizeof(int8_t));
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

    //only conditional branches will be processed   
    if (conditionRegister == 'R' && TNnotBranch != '-')
    {
        totalConBranch++;
        //Question 1: static branch predictor
        if (TNnotBranch == 'T')
            staticT++;

		
		//Question 3
        int hisLen = 8;
        int bRes = 0, gRes = 0;
        int indexQ7 = 0;
        
        //uint8_t resQ2, resQ5;
        //from 2^2 to 2^20
        for (int i = 0; i <= 20; ++i)
        {       	
        	int index = lastNbit(instructionAddress, i);
            
			bRes = (bRes << 1) | 1;
			gRes = (gRes << 1) | 1;
			//Question 2: bimodal predictor
			/*if (i == 3)
				memcpy(predB, BiCounter[i], (1<<i)*sizeof(int8_t));*/
            //0, 1 not taken; 2, 3 taken
			//prediction wrong
			if (TNnotBranch == 'T')
			{
				if (BiCounter[i][index] <= 1)
				{
					bMiss[i]++;
					bRes = bRes - 1;
				}
		
				BiCounter[i][index] = 
					satureOp(BiCounter[i][index], 1);
			}
			else if (TNnotBranch == 'N')
			{
				if (BiCounter[i][index] >= 2)
				{
					bMiss[i]++;
					bRes = bRes - 1;
				}
		
				BiCounter[i][index] = 
					satureOp(BiCounter[i][index],-1);
			}
			/*if (i == 3)
     			logFile(outputFile, pred, 1<<i, instructionAddress,
                    TNnotBranch, pred[index], bMiss[i], history, 0);*/
        	/*if (i == 3)
        		resQ2 = predB[index];*/

        //Question 3: Gshare predictor            
            int gIndex = xorNBit(index, history, hisLen>i?i:hisLen);
            
            

            //memcpy(pred, GsCounter[i], (1<<i)*sizeof(int8_t));
            //0, 1 not taken; 2, 3 taken
			//prediction wrong
			if (TNnotBranch == 'T')
			{
				if (GsCounter[i][gIndex] <= 1)
					gMiss[i]++;
		
				GsCounter[i][gIndex] = satureOp(GsCounter[i][gIndex], 1);
			}
			else if (TNnotBranch == 'N')
			{
				if (GsCounter[i][gIndex] >= 2)
					gMiss[i]++;
		
				GsCounter[i][gIndex] = satureOp(GsCounter[i][gIndex],-1);
			 }
             
             /*
             if (i == 4)
     			logFile(outputFile, pred, 1<<4, instructionAddress,
                    TNnotBranch, pred[gIndex], gMiss[i], history, hisLen);
             */
             

        //Question 5: Gshare history length is equal to predictor size
            int gIndex5 = xorNBit(index, historyQ5[i], i);
            
            /*if (i == 4)
            	memcpy(predG, GsCounterQ5[i], (1<<i)*sizeof(int8_t));*/
            //0, 1 not taken; 2, 3 taken
			//prediction wrong
			if (TNnotBranch == 'T')
			{
				if (GsCounterQ5[i][gIndex5] <= 1)
				{
					gMissQ5[i]++;
					gRes = gRes - 1;
				}
		
				GsCounterQ5[i][gIndex5] = 
					satureOp(GsCounterQ5[i][gIndex5], 1);
		
				
			}
			else if (TNnotBranch == 'N')
			{
				if (GsCounterQ5[i][gIndex5] >= 2)
				{
					gMissQ5[i]++;
					gRes = gRes - 1;
				}
		
				GsCounterQ5[i][gIndex5] = 
					satureOp(GsCounterQ5[i][gIndex5],-1);
		
				
			 }
			 /*if (i == 4){
     			logFile(outputFile, pred, 1<<4, instructionAddress,
                    TNnotBranch, pred[gIndex5], gMissQ5[i], historyQ5[i], i);}*/
             /*if (i == 4)
             {
             	historyG = historyQ5[i];
             	resQ5 = predG[gIndex5];
             }*/
            
             historyQ5[i] = histShift(historyQ5[i], TNnotBranch, i);

        //Question 6: Tournament predictor
            //memcpy(pred, ToCounterQ6[i], (1<<i)*sizeof(int8_t));
            if (ToCounterQ6[i][index] <= 1 && (bRes&1) != 1)//use bimodal, miss
            {
                tMissQ6[i]++;
            }
            else if (ToCounterQ6[i][index] >= 2 && (gRes&1) != 1)//use gshare, miss
            {
                tMissQ6[i]++;
            }
            
            if ((bRes&1) == 1 && (gRes&1) != 1)//bimodal is right, but gshare is wrong
            {
                ToCounterQ6[i][index] = satureOp(ToCounterQ6[i][index], -1);
            }
            if ((gRes&1) == 1 && (bRes&1) != 1)//gshare is right, but bimodal is wrong
            {
                ToCounterQ6[i][index] = satureOp(ToCounterQ6[i][index], 1);
            }
            
            
            /*if (i == 4){
            	fprintf(outputFile, "Bimodal: %d, Gshare: %d\n", bRes, gRes);
     			logFile(outputFile, pred, 1<<4, instructionAddress,
                    TNnotBranch, pred[gIndex], tMissQ6[i], history, 0);}*/
        //Question 7: fair tournament
        	
        	if (i >= 2)
        	{
        		indexQ7 = lastNbit(instructionAddress, i-2);
        		//memcpy(pred, ToCounterQ7[i-2], (1<<(i-2))*sizeof(int8_t));
				if (ToCounterQ7[i-2][indexQ7] <= 1 && (bRes&4) != 4)//use bimodal, miss
				{
					tMissQ7[i-2]++;
				}
				else if (ToCounterQ7[i-2][indexQ7] >= 2 && (gRes&2) != 2)//use gshare, miss
				{
					tMissQ7[i-2]++;
				}
				
				if ((bRes&4) == 4 && (gRes&2) != 2)//bimodal is right, but gshare is wrong
				{
					ToCounterQ7[i-2][indexQ7] = 
						satureOp(ToCounterQ7[i-2][indexQ7], -1);
				}
				else if ((gRes&2) == 2 && (bRes&4) != 4)//gshare is right, but bimodal is wrong
				{
					ToCounterQ7[i-2][indexQ7] = 
						satureOp(ToCounterQ7[i-2][indexQ7], 1);
				}
            }
            /*if (i == 5){
            	//fprintf(outputFile, "Bimodal: %d, Gshare: %d\n", bRes&4, gRes&2);
            	logFile(outputFile, pred, 1<<3, instructionAddress,
                    TNnotBranch, pred[indexQ7]>=2?resQ5:resQ2, tMissQ7[i-2], historyG, 4,
                     predB, predG);
            }*/
        }
        
        history = histShift(history, TNnotBranch, hisLen);

        //Question 4: Gshare history length
        
		for (int j = 10; j <= 16; j=j+6)
		{
			int bits;
			if (j == 10)
				bits = 0;
			else if (j == 16)
				bits = 1;
			int index = lastNbit(instructionAddress, j);
			
			for (int i = 0; i < 20; ++i)//history len
			{
				int gIndex = xorNBit(index, historyQ4[bits], j>i?i:j);
				
				
				//0, 1 not taken; 2, 3 taken
				//prediction wrong
				if (TNnotBranch == 'T')
				{
					if (GsCounterQ4[bits][i][gIndex] <= 1)
						gMissQ4[bits][i]++;
			
					GsCounterQ4[bits][i][gIndex] = 
						satureOp(GsCounterQ4[bits][i][gIndex], 1);
				}
				else if (TNnotBranch == 'N')
				{
					if (GsCounterQ4[bits][i][gIndex] >= 2)
						gMissQ4[bits][i]++;
			
					GsCounterQ4[bits][i][gIndex] = 
						satureOp(GsCounterQ4[bits][i][gIndex],-1);
				}
        	}
           
           	historyQ4[bits] = histShift(historyQ4[bits], TNnotBranch, 19);
        }

    }

  }

  for (int i = 0; i <= 20; ++i)
  {
      free(BiCounter[i]);
      free(GsCounter[i]);
      free(GsCounterQ5[i]);

      free(ToCounterQ6[i]);
      free(ToCounterQ7[i]);
  }
  free(BiCounter);
  free(GsCounter);
  free(GsCounterQ5);

  free(ToCounterQ6);
  free(ToCounterQ7);

  for (int j = 0; j <= 1; ++j)
  {
      for (int i = 0; i < 20; ++i)
      {
          free(GsCounterQ4[j][i]);
      }
      free(GsCounterQ4[j]);
  }
  free(GsCounterQ4);
  
  fprintf(outputFile, "Processed %" PRIi64 " trace records.\n", totalMicroops);

  fprintf(outputFile, "Micro-ops: %" PRIi64 "\n", totalMicroops);
  fprintf(outputFile, "Macro-ops: %" PRIi64 "\n", totalMacroops);

  fprintf(outputFile, "Total conditional branches: %lld\n", totalConBranch);
  fprintf(outputFile, "Q1:Static branch predictor miss rate: "
                      "Alwasy taken: %f, always not taken %f\n",
                      (double)staticT/totalConBranch,
                      1-(double)staticT/totalConBranch);

  fprintf(outputFile, "Q2:Bimodal predictor miss rates:\n"
                      "hisotry length    miss rate\n");
  for (int i = 2; i < 21; ++i)
  {
      fprintf(outputFile, "%-14d    %9f\n", 
                          i, (double)bMiss[i]/totalConBranch);
  }

  fprintf(outputFile, "Q3:Gshare predictor miss rates:\n"
                      "hisotry length    miss rate\n");
  for (int i = 2; i < 21; ++i)
  {
      fprintf(outputFile, "%-14d    %9f\n", 
                          i, (double)gMiss[i]/totalConBranch);
  }

  fprintf(outputFile, "Q4:Gshare History Length from 0 to 19 bits:\n"
                      "Hisotry Length | 2^10 miss rate | 2^16 miss rate\n");
  for (int i = 0; i < 20; ++i)
  {
      fprintf(outputFile, "%-14d   %-14f   %-14f\n",
                           i, 
                           (double)gMissQ4[0][i]/totalConBranch,
                           (double)gMissQ4[1][i]/totalConBranch);
  }
  
  fprintf(outputFile, "Q5:Gshare History Length is equal to predictor size:\n"
  					  "hisotry length    miss rate\n");
  for (int i = 2; i < 21; ++i)
  {
      fprintf(outputFile, "%-14d    %9f\n", 
                          i, (double)gMissQ5[i]/totalConBranch);
  }
  
  fprintf(outputFile, "Q6:Tournament Predictor from 2 to 20 bits\n"
  					  "hisotry length    miss rate\n");
  for (int i = 2; i < 21; ++i)
  {
      fprintf(outputFile, "%-14d    %9f\n", 
                          i, (double)tMissQ6[i]/totalConBranch);
  }
  
  fprintf(outputFile, "Q7:Tournament-fair Predictor from 2 to 20 bits\n"
  					  "hisotry length    miss rate\n");
  for (int i = 2; i < 21; ++i)
  {
      fprintf(outputFile, "%-14d    %9f\n", 
                          i, (double)tMissQ7[i-2]/totalConBranch);
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
