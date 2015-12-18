#!/bin/sh

basename=`echo $1 | sed 's/.*\\///
                         s/.tbag//'`
inputtestsdirectory="tests/"

 ./tbag < $1 2> ${basename}_compiler_output.txt

 if [$? -eq 0] then
     rm ${basename}_compiler_output.txt

     javac Driver.java

     if [[ ${basename} = *"_input"* ]]
     then
         java Driver < "$inputtestsdirectory"${basename}.in
     else
         java Driver
     fi
 else
     cat ${basename}_compiler_output.txt


rm Driver.java Item.java Npc.java Room.java *.class
