#################################################################
## Module      : Makefile
## Description : Makefile to work with the simulator of the Turing Machine.
## Copyright   : (c) Simon Stupinsky, 2020
## License     : GPL-3
## Maintainer  : xstupi00@stud.fit.vutbr.cz
## Project     : Logic project - Turing Machine
## Course      : Functional and Logic Programming (FLP)
## University  : University of Technology Brno (BUT)
## Faculty     : Faculty of Information Technology (FIT)
#################################################################

OUT=flp20-log

all:
	swipl -q -g main -o $(OUT) -c $(OUT).pl
run: all
	./$(OUT) < tests/4.in
test: all
	./tests/run_tests.sh
clean:
	rm -f $(OUT) flp-log-xstupi00.zip
pack:
	zip flp-log-xstupi00.zip Makefile $(OUT).pl tests/ README -r
