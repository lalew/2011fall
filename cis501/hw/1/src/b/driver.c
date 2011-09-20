#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

#define ARRAY_SIZE 65536

extern float x[ARRAY_SIZE];
extern float y[ARRAY_SIZE];
extern float z[ARRAY_SIZE]; 
extern float a;
extern void saxpy();

int main(int argc, char** argv)
{
  int iterations = 200000;
  if (argc > 1) {
    iterations = atoi(argv[1]);
  }
  int i;
  struct timeval start, end;
  
  for(i = 0; i < ARRAY_SIZE; i++) {
    x[i] = (float) i;
    y[i] = (float) i;
    z[i] = 0.0;
  }
  a = 1.0;
  
  printf("Starting timer\n");
  gettimeofday(&start, NULL);
  
  for(i=0; i < iterations; i++) {
    saxpy();
  }

  gettimeofday(&end, NULL);
  printf("Stopping timer\n");
  float microseconds = ((end.tv_sec - start.tv_sec) * 1000000.0) + (end.tv_usec - start.tv_usec);
  printf("%s: %f seconds\n", argv[0], microseconds / 1000000.0);
  return 0;
}
