GCC=gcc
CXX=g++
CFLAG=-Wall -Wextra -Wno-long-long -O2 -lm
UNZIP=gzcat

simulator: simulator.c
	$(CXX) $(CFLAG) -o $@ $^

simulator_d: simulator.c
	$(CXX) $(CFLAG) -D DEBUG -o $@ $^

run: sjeng-1K.trace.gz simulator
	$(UNZIP) $< | ./simulator

run2: sjeng-10M.trace.gz simulator
	$(UNZIP) $< | ./simulator

run_d: sjeng-1K.trace.gz simulator_d
	$(UNZIP) $< | ./simulator_d

run2_d: sjeng-10M.trace.gz simulator_d
	$(UNZIP) $< | ./simulator_d

clean:
	rm -f simulator simulator_d
