#!/bin/bash

gcc -std=c99 -O0 -S code.c -o code-O0.s
gcc -std=c99 -O3 -fno-tree-vectorize -S code.c -o code-O3.s 
gcc -std=c99 -O3 -funroll-loops -fno-tree-vectorize -S code.c -o code-O3-unrolled.s
gcc -std=c99 -O3 -funroll-loops -ftree-vectorize -S code.c -o code-O3-unrolled-vector.s
gcc -std=c99 -O3 -funroll-loops -ftree-vectorize -c -fopenmp code.c -o code-O3-unrolled-vector-openmp.o

gcc -static code-O0.s driver.c -o code-O0
gcc -static code-O3.s driver.c -o code-O3
gcc -static code-O3-unrolled.s driver.c -o code-O3-unrolled
gcc -static code-O3-unrolled-vector.s driver.c -o code-O3-unrolled-vector
gcc -static code-O3-unrolled-vector-openmp.o driver.c -o code-O3-unrolled-vector-openmp -fopenmp
