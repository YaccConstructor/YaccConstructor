
#include "cuda_runtime.h"
#include "device_launch_parameters.h"

#include <stdio.h>
#include <iostream>
using namespace std;

typedef unsigned long long uint64;
typedef unsigned long uint32;

typedef uint64 rule;
typedef uint32 token;

typedef uint64 cellData;

//(16|   16| 16|8       |       8  |)
//(A |->  B|  C|<lblName|lblWeight>|)
__device__ int getRuleA(rule r){return r >> 48;}
__device__ int getRuleB(rule r){return r >> 32 & 0xFFFF;}
__device__ int getRuleC(rule r){return r >> 16 & 0xFFFF;}
__device__ int getRuleN(rule r){return r >> 8 & 0xFF;}
__device__ int getRuleW(rule r){return r & 0xFF;}

__host__ cellData buildRule(int A, int B, int C, int lblName, int lblWeght){
	return (uint64)A << 48 | (uint64)B << 32 | (uint64)C << 16 | lblName << 8 | lblWeght;
}

//(16 |16                |16       |8       |8        )
//(k  |non-terminalIndex |lblState |lblName |lblWeght )
__device__ int getDataK(cellData d){return d >> 48;}
__device__ int getDataI(cellData d){return d >> 32 & 0xFFFF;}
__device__ int getDataS(cellData d){return d >> 16 & 0xFFFF;}
__device__ int getDataN(cellData d){return d >> 8 & 0xFF;}
__device__ int getDataW(cellData d){return d & 0xFF;}


__host__ cellData buildData_Host(int ruleIndex){return (uint64)ruleIndex << 32;}
__device__ cellData buildData(int k, int ruleIndex, int lblState, int lblName, int lblWeght){
	return (uint64)k << 48 | (uint64)ruleIndex << 32 | lblState << 16 | lblName << 8 | lblWeght;
}

__device__ int getCellRuleTop (rule* rules, cellData* cellContent, int i){
	return getDataI( cellContent[ cellContent[0] - i ] );
}

__device__ void processRule(rule* rules, int rulesCount, rule currentRule, int start, int k, int strLen, int subLen, cellData* table){
	// subLen <-- l
	// start <-- i

	int b = getRuleB(currentRule);
	int c = getRuleC(currentRule);
	int rn = getRuleN(currentRule);
	int rw = getRuleW(currentRule);

	if( c != 0 ){
		cellData *current = table + (			subLen * strLen + start			  ) * (rulesCount + 1);
		cellData *left	  = table + (				 k * strLen + start			  ) * (rulesCount + 1);
		cellData *right	  = table + ( (subLen - k - 1) * strLen + (k + start + 1) ) * (rulesCount + 1);
		int count1 = left[0];
		int count2 = right[0];

		if(count1 > 0 && count2 > 0){
			for(int m = 0; m < count1; m++){
				for(int n = 0; n < count2; n++){
					if ( getCellRuleTop(rules, left, m) == b && getCellRuleTop(rules, right, n) == c ){
						cellData cellData1 = left[ left[0] - m ];
						cellData cellData2 = right[ right[0] - n ];

						/*
						int lState1 = getDataS(cellData1);
						int lbl1 = getDataN(cellData1);
						int weight1 = getDataW(cellData1);

						int lState2 = getDataS(cellData2);
						int lbl2 = getDataN(cellData2);
						int weight2 = getDataW(cellData2);

						int newLabel, newlState, newWeight
						*/

						cellData currentElem = buildData(k,getRuleA(currentRule),0,0,0);

						current[0]++;
						current[ getRuleA(currentRule) ] = currentElem;
					}
				}
			}
		}
	}
}

__global__ void elem(rule* rules, int rulesCount, int* index, int strLen, int subLen, cellData* table){
	int nonTerminal = threadIdx.x;
	int i = index[nonTerminal] + threadIdx.y;
	if( i >= index[nonTerminal+1] ) return;
	int start = blockIdx.x;

	for(int k = 0; k < subLen; k++){
		processRule(rules, rulesCount, rules[i], start, k, strLen, subLen, table);
	}
}

__global__ void compress(int rulesCount, int strLen, int subLen, cellData* table){
	int start = blockIdx.x;

	cellData *current = table + (subLen * strLen + start) * (rulesCount + 1);

	current[0]=0;
	int j = 1;
	for(int i = 1; i <= rulesCount; i++ ){
		if(current[i]){
			current[j] = current[i];
			j++;
			//current[i] = 0;
			current[0]++;
		}
	}
}

//__global__ void fillRow(rule* rules, int rulesCount, int strLen, int subLen, cellData* table){
//	for(int start = 0; start < strLen - subLen; start++){
//		elem(rules, rulesCount, start, strLen, subLen, table);
//	}
//}

__host__ void fillTable(rule* rules, int rulesCount, int* index, int nCount, int strLen, cellData* table){
    cellData *dev_table = 0;
	rule *dev_rules = 0;
	int *dev_index = 0;

	int table_size = strLen * strLen * (rulesCount+1);

    cudaError_t cudaStatus;
    cudaStatus = cudaSetDevice(0);

    cudaStatus = cudaMalloc((void**)&dev_table, table_size * sizeof(cellData));
    cudaStatus = cudaMalloc((void**)&dev_rules, rulesCount * sizeof(rule));
    cudaStatus = cudaMalloc((void**)&dev_index, (nCount + 1) * sizeof(int));
    cudaStatus = cudaMemcpy(dev_table, table, table_size * sizeof(cellData), cudaMemcpyHostToDevice);
    cudaStatus = cudaMemcpy(dev_rules, rules, rulesCount * sizeof(rule), cudaMemcpyHostToDevice);
    cudaStatus = cudaMemcpy(dev_index, index, (nCount + 1) * sizeof(int), cudaMemcpyHostToDevice);

	for(int subLen = 1; subLen <= strLen; subLen++){
		elem<<<strLen-subLen, dim3(nCount,2,1)>>>(dev_rules, rulesCount, dev_index, strLen, subLen, dev_table);
		compress<<<strLen-subLen, 1>>>(rulesCount, strLen, subLen, dev_table);
	}

    cudaStatus = cudaDeviceSynchronize();
    cudaStatus = cudaMemcpy(table, dev_table, table_size * sizeof(cellData), cudaMemcpyDeviceToHost);

    cudaFree(dev_table);
    cudaFree(dev_rules);
    cudaFree(dev_index);
}

int main(){
	const int rulesCount = 5;
	const int wordLen = 5;
	const int nCount = 4;

	rule *rules = new rule[rulesCount];
	rules[0] = buildRule(1,2,1,0,0);
	rules[1] = buildRule(2,3,3,0,0);
	rules[2] = buildRule(3,1,2,0,0);
	rules[3] = buildRule(4,1,2,0,0);
	rules[4] = buildRule(4,2,3,0,0);
	//rules[5] = buildRule(1,11,0,0,0);
	//rules[6] = buildRule(2,12,0,0,0);
	//rules[7] = buildRule(3,11,0,0,0);

	int index[nCount+1] = {0,1,2,3,rulesCount};

	cellData* table = new cellData[wordLen * wordLen * (rulesCount + 1)];
	for(int i = 0; i < wordLen * wordLen * (rulesCount + 1); i++)
		table[i] = 0;

	table[0*(rulesCount+1) + 0] = 1;
	table[0*(rulesCount+1) + 1] = buildData_Host(2);

	table[1*(rulesCount+1) + 0] = 2;
	table[1*(rulesCount+1) + 1] = buildData_Host(1);
	table[1*(rulesCount+1) + 2] = buildData_Host(3);

	table[2*(rulesCount+1) + 0] = 2;
	table[2*(rulesCount+1) + 1] = buildData_Host(1);
	table[2*(rulesCount+1) + 2] = buildData_Host(3);

	table[3*(rulesCount+1) + 0] = 1;
	table[3*(rulesCount+1) + 1] = buildData_Host(2);

	table[4*(rulesCount+1) + 0] = 2;
	table[4*(rulesCount+1) + 1] = buildData_Host(1);
	table[4*(rulesCount+1) + 2] = buildData_Host(3);


	fillTable(rules, rulesCount, index, nCount, wordLen, table);


	for(int i = 0; i < wordLen; i++){
		for(int j = 0; j < wordLen*(rulesCount+1); j++){
			if( !( j % (rulesCount+1) ) && j ) cout<<' ';
			if( !( j % (rulesCount+1) ) )
				cout<<(table[i*wordLen*(rulesCount+1)+j]);
			else
				cout<<(table[i*wordLen*(rulesCount+1)+j] >> 32 & 0xFFFF);		
			if( !( j % (rulesCount+1) ) ) cout<<' ';
		}
		cout<<endl;
	}

	system("pause");
	return 0;
}