#!/bin/bash
#cat $1 >> full.txt
#cat lib/stdlib.tbag >> full.txt
python scripts/importLibrary.py $1

./tbag < prog_w_stdlib.tbag
rm prog_w_stdlib.tbag
javac Driver.java
javac Room.java
java Driver