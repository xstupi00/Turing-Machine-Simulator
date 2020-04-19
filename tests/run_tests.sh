#!/bin/bash

#################################################################
# Module      : run_tests.sh
# Description : Test script to automated testing of the Turing Machine simulator.
# Copyright   : (c) Simon Stupinsky, 2020
# License     : GPL-3
# Maintainer  : xstupi00@stud.fit.vutbr.cz
# Project     : Logic project - Turing Machine
# Course      : Functional and Logic Programming (FLP)
# University  : University of Technology Brno (BUT)
# Faculty     : Faculty of Information Technology (FIT)
#################################################################

script_location=$(dirname $0)
path_to_executable="$script_location/../flp20-log"

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

index=1
zero=0
if [ $index -lt 10 ] ;
then
  nextFile="$script_location/$zero$index.in"
  solution="$script_location/$zero$index.out"
else
  nextFile="$script_location/$index.in"
  solution="$script_location/$index.out"
fi

while [ -e "$nextFile" ]; do

  output=$("$path_to_executable" <"$nextFile")
  if difference=$(diff --side-by-side <(echo "$output") "$solution"); then 
    echo -e "Test $index -${GREEN} OK ${NC}" >&2
  else 
    echo -e "Test $index -${RED} FAIL ${NC} Program's vs expected output:\n\n$difference\n" >&2
  fi

  index=$((index+1))

  if [ $index -lt 10 ] ;
  then
    nextFile="$script_location/$zero$index.in"
    solution="$script_location/$zero$index.out"
  else
    nextFile="$script_location/$index.in"
    solution="$script_location/$index.out"
  fi

done
