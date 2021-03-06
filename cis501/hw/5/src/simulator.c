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
#include <deque>

using std::queue;
using std::deque;

#define RFLAG     49
#define AREG_SIZE 50
#define PREG_SIZE 2048
#define ROB_SIZE  1024
#define ISSUE_WD  8
#define RESTART_PENALTY 5

int64_t totalMicroops = 0;
int64_t totalMacroops = 0;
int64_t totalCycle = 0;

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
    int64_t fetch_cycle;
    int64_t issue_cycle;
    int64_t done_cycle;
    int64_t commit_cycle;
    int64_t numMicro;
    int latency;

    int is_load;//0--not, 1--load
    int is_store;//0--not, 1--store

    uint64_t instrAddr;
    char TN;
    int is_mis_pred;
} Microop;

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

//0--mis-prediction, 1--right
int bimodal(uint64_t instrAddr, char TNnotBranch)
{
    static int addr_len = 16;
    static int8_t BiCounter[1<<16];

    int pred_res = 1;

    int index = lastNbit(instrAddr, addr_len);

    if (TNnotBranch == 'T')
    {
        if (BiCounter[index] <= 1)
            pred_res = 0;

        BiCounter[index] = satureOp(BiCounter[index], 1);
    }
    else if (TNnotBranch == 'N')
    {
        if (BiCounter[index] >= 2)
            pred_res = 0;

        BiCounter[index] = satureOp(BiCounter[index], -1);
    }
    //16KB predicotr, 2^16 entries
    //see hw3 for code

    return pred_res;
}

//0--mis-prediction, 1--right
int gshare(uint64_t instrAddr, char TNnotBranch)
{
    //16bit history
    static int32_t history;
    static int addr_len = 16;
    static int8_t GsCounter[1<<16];

    int pred_res = 1;//right

    int index = lastNbit(instrAddr, addr_len);

    int gIndex = xorNBit(index, history, addr_len); 

    if (TNnotBranch == 'T')
    {
        if (GsCounter[gIndex] <= 1)
            pred_res = 0;
        GsCounter[gIndex] = satureOp(GsCounter[gIndex], 1);
    }
    else if (TNnotBranch == 'N')
    {
        if (GsCounter[gIndex] >= 2)
            pred_res = 0;
        GsCounter[gIndex] = satureOp(GsCounter[gIndex], -1);
    }

    history = histShift(history, TNnotBranch, addr_len);

    return pred_res;
}

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

void commit(const Microop &pInstr, queue<uint32_t> &freelist)
{
    if (pInstr.arch_dest != -1)
        freelist.push(pInstr.phys_dest_ex);
    if (pInstr.writeFlag == 1)
        freelist.push(pInstr.flag_phys_ex);
}

bool isReady(const Microop &pInstr, int *scoreboard, deque<Microop> &ROB)
{
    //exp1 requirement
    if (pInstr.arch_src1 != -1 && 
        scoreboard[pInstr.phys_src1] != 0)
        return false;
    if (pInstr.arch_src2 != -1 && 
        scoreboard[pInstr.phys_src2] != 0)
        return false;
    if (pInstr.readFlag == 1 && 
        scoreboard[pInstr.flag_phys_ex] != 0)
        return false;

    //exp2 requirement
    if (pInstr.is_load == 1)
    {
        deque<Microop>::iterator it;
        for (it = ROB.begin(); it != ROB.end(); ++it)
            if ((*it).is_store == 1 &&
                (*it).numMicro < pInstr.numMicro &&
                !((*it).issued ==1 && (*it).done_cycle <= totalCycle))
            {
                //exp3 requirement
                if ((*it).mem_op_addr == pInstr.mem_op_addr)
                    return false;
            }
    }


    return true;


}

void fetch(Microop &pInstr, int &endOfTrace, FILE *inputFile)
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
      endOfTrace = 1;
      return;
    }

    if (result != 14) {
      fprintf(stderr, "Error parsing trace at line %" PRIi64 "\n",             
              totalMicroops);
      abort();
    }

    // For each micro-op
    totalMicroops++;

    // For each macro-op:
    if (microOpCount == 1) {
      totalMacroops++;
    }

    //additional work
    pInstr.writeFlag = 0;//whether flag will be written or not
    pInstr.readFlag = 0;//flag is read or not
    strncpy(pInstr.microOp, microOperation, 23);
    strncpy(pInstr.macroOp, macroOperation, 12);

    pInstr.arch_src1 = sourceRegister1;
    pInstr.arch_src2 = sourceRegister2;
    pInstr.arch_dest = destinationRegister;
    if (conditionRegister == 'W')//write flag, need new register for r49
    {
        pInstr.writeFlag = 1;
    }
    if (conditionRegister == 'R')
    {
        pInstr.readFlag = 1;
    }

    if (loadStore == 'L')
        pInstr.latency = 4;
    else
        pInstr.latency = 1;

    pInstr.fetch_cycle = totalCycle;
    pInstr.numMicro = totalMicroops;

    if (loadStore == 'L')
        pInstr.is_load = 1;
    if (loadStore == 'S')
        pInstr.is_store = 1;

    if (loadStore != '-')
        pInstr.mem_op_addr = addressForMemoryOp;

    pInstr.instrAddr = instructionAddress;
    pInstr.TN = TNnotBranch;
}

void log_info(const Microop &pInstr, 
              FILE* outputFile)
{
    fprintf(outputFile, "%lld", pInstr.numMicro);

    fprintf(outputFile, ": %lld %lld %lld %lld",
            pInstr.fetch_cycle, pInstr.issue_cycle,
            pInstr.done_cycle, pInstr.commit_cycle);

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
  
  uint32_t maptable[AREG_SIZE];//architecture register from 0 to AREG_SIZE-1
  queue<uint32_t> freelist;//free list from AREG_SIZE to PREG_SIZE-1
  deque<Microop> ROB;//a list of reorder buffer
  int fetch_ready = 0;
  int scoreboard[PREG_SIZE];

  memset(scoreboard, 0, sizeof(int)*PREG_SIZE);


  for (int i = 0; i < AREG_SIZE; ++i)
  {
      maptable[i] = i;
  }

  for (int i = AREG_SIZE; i < PREG_SIZE; ++i)
  {
      freelist.push(i);
  }

  fprintf(outputFile, "Processing trace...\n");

  int endOfTrace = 0;
  int endOfROB = 0;

  while (!(endOfTrace && endOfROB)) {
    //1.commit
    for (int i = 0; i < ISSUE_WD; ++i)
    {
        if (!ROB.empty() && 
            ROB.front().issued == 1 && 
            ROB.front().done_cycle <= totalCycle)
        {
            ROB.front().commit_cycle = totalCycle;
#ifdef DEBUG
            log_info(ROB.front(), outputFile);
#endif            
            commit(ROB.front(), freelist);
            ROB.pop_front();
        }

        if (endOfTrace == 1 && ROB.empty())
        {
            endOfROB = 1;
            break;
        }
    }
    

    //2.issue
    int issueCnt = 0;
    deque<Microop>::iterator it;
    for (it = ROB.begin(); it != ROB.end(); it++)
    {
        if ((*it).issued == 0 && isReady(*it, scoreboard, ROB))
        {
            (*it).issued = 1;
            (*it).issue_cycle = totalCycle;
            (*it).done_cycle = totalCycle + (*it).latency;

            //write to some reg
            if ((*it).arch_dest != -1)
                scoreboard[(*it).phys_dest] = (*it).latency;
            //write the condition
            if ((*it).writeFlag == 1)
                scoreboard[(*it).flag_phys] = (*it).latency;

            //exp 4, 5
            if ((*it).is_mis_pred == 1)
            {
                assert(fetch_ready == -1);
                fetch_ready = (*it).latency + RESTART_PENALTY;
            }

            issueCnt++;

            if (issueCnt == ISSUE_WD)
                break;
        }
    }


    //fetch & rename
    if (!endOfTrace)
    {
        for (int i = 0; i < ISSUE_WD; ++i)
        {
            if (fetch_ready > 0)
            {
                fetch_ready = fetch_ready -1;
                break;
            }
            if (fetch_ready == -1)
                break;

            assert(fetch_ready == 0);


            if (ROB.size() == ROB_SIZE)
                break;

            Microop aInstr;

            memset(&aInstr, 0, sizeof(Microop));

            fetch(aInstr, endOfTrace, inputFile);

            //exp4
            if (aInstr.readFlag == 1 && aInstr.TN != '-')
            {
                //int pred_res = bimodal(aInstr.instrAddr, aInstr.TN);
                int pred_res = gshare(aInstr.instrAddr, aInstr.TN);
                if (pred_res == 0)//misprediction
                {
                    fetch_ready = -1;
                    aInstr.is_mis_pred = 1;
                }
            }
            //exp5
            
            if (endOfTrace)
                break;

            reg_rename(aInstr, maptable, freelist);
            ROB.push_back(aInstr);

            //set not ready for dest reg in scoreboard
            if (aInstr.arch_dest != -1)
                scoreboard[aInstr.phys_dest] = -1;
            if (aInstr.writeFlag == 1)
                scoreboard[aInstr.flag_phys] = -1;
        }
    }

    totalCycle++;
    //printf("scoreboard:%d",totalCycle);
    for (int i = 0; i < PREG_SIZE; ++i)
    {
        if (scoreboard[i] > 0)
        {
            scoreboard[i]--;
        }

        //if (scoreboard[i] != 0)
        //    printf(", p%d: %d", i, scoreboard[i]);
    }

    //printf("\n");
  }
  
  fprintf(outputFile, "Processed %" PRIi64 " trace records.\n", totalMicroops);

  fprintf(outputFile, "Micro-ops: %" PRIi64 "\n", totalMicroops);
  fprintf(outputFile, "Macro-ops: %" PRIi64 "\n", totalMacroops);

  fprintf(outputFile, "TotalCycles: %lld\n", totalCycle);

  fprintf(outputFile, "uIPC:%f\n", (double)totalMicroops/totalCycle);
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
