#!/bin/sh

basename=`echo $1 | sed 's/.*\\///
                         s/.tbag//'`
./tbag < $1 > ${basename}.java
javac ${basename}.java
java ${basename} 
rm ${basename}.java
