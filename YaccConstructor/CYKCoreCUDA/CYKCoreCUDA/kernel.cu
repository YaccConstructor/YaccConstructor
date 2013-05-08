
#include "cuda_runtime.h"
#include "device_launch_parameters.h"

#include <stdio.h>
#include <iostream>
using namespace std;

typedef unsigned long long uint64;
typedef unsigned long uint16;

typedef uint64 rule;

typedef uint16 cellData;

//(16|   16| 16|8       |       8  |)
//(A |->  B|  C|<lblName|lblWeight>|)
__device__ int getRuleA(rule r){return r >> 48;}
__device__ int getRuleB(rule r){return r >> 32 & 0xFFFF;}
__device__ int getRuleC(rule r){return r >> 16 & 0xFFFF;}
__device__ int getRuleN(rule r){return r >> 8 & 0xFF;}
__device__ int getRuleW(rule r){return r & 0xFF;}

__host__ rule buildRule(int A, int B, int C, int lblName, int lblWeght){
	return (uint64)A << 48 | (uint64)B << 32 | (uint64)C << 16 | lblName << 8 | lblWeght;
}

//(16 |16                |16       |8       |8        )
//(k  |non-terminalIndex |lblState |lblName |lblWeght )
__device__ int getDataI(cellData d){return d;}

__host__ cellData buildData_Host(int ruleIndex){return ruleIndex;}
__device__ cellData buildData(int k, int ruleIndex, int lblState, int lblName, int lblWeght){
	return ruleIndex;
}

__global__ void processRule(rule* rules, int rulesCount, int nCount, int strLen, int subLen, cellData* table){
	// subLen === l
	// start === i
	int start = blockIdx.y * blockDim.y + threadIdx.y;
	if(start >= strLen - subLen) return;
	rule currentRule = rules[threadIdx.x];

	for(int k = 0; k < subLen; k++){	
		cellData *current = table + (			subLen * strLen + start			  ) * (nCount + 1);
		cellData *left	  = table + (				 k * strLen + start			  ) * (nCount + 1);
		cellData *right	  = table + ( (subLen - k - 1) * strLen + (k + start + 1) ) * (nCount + 1);
		
		int c = getRuleC(currentRule);
		if(current[ getRuleA(currentRule) ]) return;

		for(int m = 1; m <= nCount; m++){
			if ( getDataI( left[m] ) == getRuleB(currentRule) ){
				for(int n = 1; n <= nCount; n++){
					if ( getDataI( right[n] ) == c ){							
						current[ getRuleA(currentRule) ] = getRuleA(currentRule);
					}
				}
			}	
		}
	}
	__syncthreads();
}

__host__ void fillTable(rule* rules, int rulesCount, int nCount, int strLen, cellData* table){
	cudaEvent_t start, stop;
    float gpuTime = 0.0f;

	int deviceCount;
	cudaDeviceProp cdp;
	cudaGetDeviceProperties ( &cdp, 0 );

    cudaEventCreate ( &start );
    cudaEventCreate ( &stop );
	cudaEventRecord ( start, 0 );

    cellData *dev_table = 0;
	rule *dev_rules = 0;
	int table_size = strLen * strLen * (nCount+1);
    cudaError_t cudaStatus;

    cudaStatus = cudaSetDevice(0);

    cudaStatus = cudaMalloc((void**)&dev_table, table_size * sizeof(cellData));
    cudaStatus = cudaMalloc((void**)&dev_rules, rulesCount * sizeof(rule));
    cudaStatus = cudaMemcpy(dev_table, table, table_size * sizeof(cellData), cudaMemcpyHostToDevice);
    cudaStatus = cudaMemcpy(dev_rules, rules, rulesCount * sizeof(rule), cudaMemcpyHostToDevice);

	int threadsPerBlockX = cdp.maxThreadsPerBlock / rulesCount;

	for(int subLen = 1; subLen <= strLen; subLen++){
		processRule<<< dim3( 1,(strLen-subLen)/(threadsPerBlockX)+1 ), dim3(rulesCount,  threadsPerBlockX ) >>>(dev_rules, rulesCount, nCount, strLen, subLen, dev_table);
	}

    cudaStatus = cudaDeviceSynchronize();
    cudaStatus = cudaMemcpy(table, dev_table, table_size * sizeof(cellData), cudaMemcpyDeviceToHost);

	cudaEventRecord (stop, 0);

	cudaEventSynchronize ( stop );
	cudaEventElapsedTime ( &gpuTime, start, stop );

	cout<<"time "<< gpuTime<<endl;
	
    cudaFree(dev_table);
    cudaFree(dev_rules);
}

int main(){
	int rulesCount = 7;
	int wordLen = 1000;
	int nCount = 4;

	rule *rules = new rule[rulesCount];
	rules[0] = buildRule(1,2,3,0,0);
	rules[1] = buildRule(2,3,2,0,0);
	rules[2] = buildRule(2,3,3,0,0);
	rules[3] = buildRule(3,1,2,0,0);
	rules[4] = buildRule(4,2,4,0,0);
	rules[5] = buildRule(1,4,2,0,0);
	rules[6] = buildRule(3,4,2,0,0);

	cellData* table = new cellData[wordLen * wordLen * (nCount + 1)];
	for(int i = 0; i < wordLen * wordLen * (nCount + 1); i++)
		table[i] = 0;

	for(int i=0; i<wordLen; i++){
		table[i*(nCount+1) + 0] = 1;
		table[i*(nCount+1) + 3] = buildData_Host(3);
	}

	table[(wordLen-2)*(nCount+1) + 0] = 1;
	table[(wordLen-2)*(nCount+1) + 2] = buildData_Host(2);
	table[(wordLen-2)*(nCount+1) + 3] = buildData_Host(0);

	table[(wordLen-5)*(nCount+1) + 0] = 1;
	table[(wordLen-5)*(nCount+1) + 4] = buildData_Host(4);
	table[(wordLen-5)*(nCount+1) + 3] = buildData_Host(0);

	fillTable(rules, rulesCount, nCount, wordLen, table);

	//for(int i = 0; i < wordLen; i++){
	//	for(int j = 0; j < wordLen*(nCount+1); j++){
	//		if( !( j % (nCount+1) ) && j ) cout<<' ';
	//		if( !( j % (nCount+1) ) )
	//			cout<<"";//(table[i*wordLen*(nCount+1)+j]);
	//		else
	//			cout<<(table[i*wordLen*(nCount+1)+j]);		
	//		//if( !( j % (nCount+1) ) ) cout<<' ';
	//	}
	//	cout<<endl;
	//}

	for(int i = wordLen-1; i < wordLen; i++){
		for(int j = 0; j < 1*(nCount+1); j++){
			if( !( j % (nCount+1) ) && j ) cout<<' ';
			if( !( j % (nCount+1) ) )
				cout<<(table[i*wordLen*(nCount+1)+j]);
			else
				cout<<(table[i*wordLen*(nCount+1)+j] >> 32 & 0xFFFF);		
			if( !( j % (nCount+1) ) ) cout<<' ';
		}
		cout<<endl;
	}

	system("pause");
	return 0;
}