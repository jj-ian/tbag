#!/bin/sh

basename=`echo $1 | sed 's/.*\\///
                         s/.tbag//'`
inputtestsdirectory="tests/"
driverfile="Driver.java"

# clean up all existing .java files
rm -f Driver.java Item.java Npc.java Room.java *.class

./tbag < $1 > ${basename}_compiler_output.txt 2>&1

if [ -f $driverfile ]; then
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
fi

rm -f Driver.java Item.java Npc.java Room.java *.class
