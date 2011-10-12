GCC=gcc
CFLAG=-Wall -Wextra -pedantic -std=c99 -O2 -lm -D LSTALL=1
UNZIP=gzcat

simulator: simulator.c
	$(GCC) $(CFLAG) -o $@ $^

simulator_d: simulator.c
	$(GCC) $(CFLAG) -D DEBUG -o $@ $^

run: sjeng-1K.trace.gz simulator
	$(UNZIP) $< | ./simulator

run2: sjeng-10M.trace.gz simulator
	$(UNZIP) $< | ./simulator

run_d: sjeng-1K.trace.gz simulator_d
	$(UNZIP) $< | ./simulator_d

run2_d: sjeng-10M.trace.gz simulator_d
	$(UNZIP) $< | ./simulator_d

clean:
	rm -f simulator
