#include <omp.h>

#define ARRAY_SIZE 65536

float x[ARRAY_SIZE];
float y[ARRAY_SIZE];
float z[ARRAY_SIZE]; 
float a;

void saxpy()
{
#pragma omp parallel for
  for(int i = 0; i < ARRAY_SIZE; i++) {
    z[i] = a*x[i] + y[i];
  }
}

