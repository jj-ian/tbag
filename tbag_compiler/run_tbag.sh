#!/bin/bash
cat $1 >> full.txt
cat tbag_lib.txt >> full.txt
./tbag < full.txt
rm full.txt
javac Driver.java
javac Room.java
java Driver