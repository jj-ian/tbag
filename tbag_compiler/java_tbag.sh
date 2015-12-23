#!/bin/sh
# Contributions from Maria, Julie

basename=`echo $1 | sed 's/.*\\///
                         s/.tbag//'`
inputtestsdirectory="tests/"
driverfile="Driver.java"

# clean up all existing .java files
rm -f Driver.java Item.java Npc.java Room.java *.class

# add tbag standard library to the end of the file
python scripts/importLibrary.py $1

./tbag < prog_w_stdlib.tbag > ${basename}_compiler_output.txt 2>&1
rm prog_w_stdlib.tbag

if [ -f $driverfile ]; then

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

rm ${basename}_compiler_output.txt
rm -f Driver.java Item.java Npc.java Room.java *.class
