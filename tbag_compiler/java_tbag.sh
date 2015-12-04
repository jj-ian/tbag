#!/bin/sh

basename=`echo $1 | sed 's/.*\\///
                         s/.tbag//'`
./tbag < $1
javac Driver.java
java Driver
rm Driver.java Item.java Npc.java Room.java *.class
