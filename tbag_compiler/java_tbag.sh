#!/bin/sh

basename=`echo $1 | sed 's/.*\\///
                         s/.tbag//'`
inputtestsdirectory="tests/"

 ./tbag < $1
javac Driver.java

if [[ ${basename} = *"_input"* ]]
then
    java Driver < "$inputtestsdirectory"${basename}.in
else
    java Driver
fi

rm Driver.java Item.java Npc.java Room.java *.class
