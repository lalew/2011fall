GCC=gcc
CFLAG=-Wall -Wextra -pedantic -std=c99 -O2 -lm
UNZIP=gzcat

simulator: simulator.c
	$(GCC) $(CFLAG) -o $@ $^

run: sjeng-1K.trace.gz simulator
	$(UNZIP) $< | ./simulator
