#!/bin/sh

TBAG="./tbag"

# Set time limit for all operations
ulimit -t 30

globallog=testall.log
rm -f $globallog
error=0
globalerror=0

keep=0

Usage() {
    echo "Usage: testall.sh [options] [.tbag files]"
    echo "-k    Keep intermediate files"
    echo "-h    Print this help"
    exit 1
}

SignalError() {
    if [ $error -eq 0 ] ; then
	echo "FAILED"
	error=1
    fi
    echo "  $1"
}

# Compare <outfile> <reffile> <difffile>
# Compares the outfile with reffile.  Differences, if any, written to difffile
Compare() {
    generatedfiles="$generatedfiles $3"
    echo diff -b $1 $2 ">" $3 1>&2
    diff -b "$1" "$2" > "$3" 2>&1 || {
	SignalError "$1 differs"
	echo "FAILED $1 differs from $2" 1>&2
    }
}

# Run <args>
# Report the command, run it, and report any errors
Run() {
    echo $* 1>&2
    eval $* || {
	SignalError "$1 failed on $*"
	return 1
    }
}

Check() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.tbag//'`
    reffile=`echo $1 | sed 's/.tbag$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    echo "basename: " $basename
    echo "ref file: " $reffile
    echo "basedir": $basedir
    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""

    generatedfiles="$generatedfiles ${basename}.java.out" &&
    Run "$TBAG" "<" $1 ">" ${basename}.java.out &&
    Compare ${basename}.java.out ${reffile}.java ${basename}.diff
    #Compare ${basename}.java ${reffile}.java ${basename}.diff


    # Report the status and clean up the generated files
    echo "generared files" $generatedfiles
    if [ $error -eq 0 ] ; then
	if [ $keep -eq 0 ] ; then
	    rm -f $generatedfiles
	fi
	echo "OK"
	echo "###### SUCCESS" 1>&2
    else
	echo "###### FAILED" 1>&2
	globalerror=$error
    fi
}

while getopts kdpsh c; do
    case $c in
	k) # Keep intermediate files
	    keep=1
	    ;;
	h) # Help
	    Usage
	    ;;
    esac
done

shift `expr $OPTIND - 1`

if [ $# -ge 1 ]
then
    files=$@
else
    #files="tests/fail-*.mc tests/test-*.mc"
    files="tests/test-*.tbag"
fi

for file in $files
do
    echo $file
    case $file in
	*test-*)
	    Check $file 2>> $globallog
	    ;;
#	*fail-*)
#	    CheckFail $file 2>> $globallog
#	    ;;
	*)
	    echo "unknown file type $file"
	    globalerror=1
	    ;;
    esac
done

exit $globalerror
