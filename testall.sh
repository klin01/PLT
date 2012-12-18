#!/bin/ksh

RETROCRAFT="./retrocraft"

# Set time limit for all operations
ulimit -t 30

globallog=testall.log
rm -f $globallog
error=0
globalerror=0

keep=0
createref=0

Usage() {
    echo "Usage: testall.sh [options] [.rc files]"
    echo "-k    Keep intermediate files"
	echo "-r    Generate test references instead of running code against them"
    echo "-h    Print this help"
    exit 1
}

SignalError() {
    if [ $error -eq 0 ] ; then
	echo "###### FAILED"
	error=1
    fi
    echo "###### $1"
}

# Compare <outfile> <reffile> <difffile>
# Compares the outfile with reffile.  Differences, if any, written to difffile
Compare() {
    generatedfiles="$generatedfiles $3"
    echo diff -b $1 $2 ">" $3 1>&2
    diff -b "$1" "$2" > "$3" 2>&1 || {
	SignalError "$1 differs"
	echo "###### FAILED $1 differs from $2" 1>&2
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
                             s/.rc//'`
    reffile=`echo $1 | sed 's/.rc$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."




    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""

	if [$createref -eq 1] ; then
		echo -n "HERE1 refence: $createref"
	else
		echo -n "HERE2 refence: $createref"
	fi

	echo -n "refence: $createref"
	if [ $createref -eq 1 ] ; then
		echo -n "Creating $reffile.reference"
		generatedfiles="$generatedfiles ${basename}.reference" &&
		Run "$RETROCRAFT" "-c" "<" $1 ">" ${reffile}.reference
	else
		if [ $createref -eq 0 ] ; then
			echo -n "Testing $reffile.rc against $reffile.reference"
			generatedfiles="$generatedfiles ${basename}.c.out" &&
			Run "$RETROCRAFT" "-c" "<" $1 ">" ${basename}.c.out &&
			Compare ${basename}.c.out ${reffile}.reference ${basename}.c.diff
		fi
	fi

    # COMMENT this when generate reference!
    # This section runs the code against the existing reference
    # and return OK if the results are the same.
	#
	# echo -n "Testing $reffile.rc against $reffile.reference"
    #generatedfiles="$generatedfiles ${basename}.c.out" &&
   # Run "$RETROCRAFT" "-c" "<" $1 ">" ${basename}.c.out &&
    #Compare ${basename}.c.out ${reffile}.reference ${basename}.c.diff

    # UNCOMMENT this part to make reference files!
	# COMMENT the part above when making reference too!
    #
	# echo -n "Creating $reffile.reference"
    # generatedfiles="$generatedfiles ${basename}.reference" &&
    # Run "$RETROCRAFT" "-c" "<" $1 ">" ${reffile}.reference

    # Report the status and clean up the generated files

    if [ $error -eq 0 ] ; then
	if [ $keep -eq 0 ] ; then
	    rm -f $generatedfiles
	fi
	echo "RESULT: OK"
	echo "###### SUCCESS" 1>&2
    else
	echo "###### FAILED" 1>&2
	globalerror=$error
    fi
}

while getopts ":rkh" c; do
    case $c in
	r) 
		createref=1
		;;
	k) 
		keep=1
		;;
	h) 
		Usage
		;;
    esac
done

shift `expr $OPTIND - 1`

if [ $# -ge 1 ]
then
    files=$@
else
    files="tests/fail-*.mc tests/Basic/test-*.rc tests/Array/test-*.rc"
fi

for file in $files
do
    case $file in
	*test-*)
	    Check $file 2>> $globallog
	    ;;
	*fail-*)
	    CheckFail $file 2>> $globallog
	    ;;
	*)
	    echo "unknown file type $file"
	    globalerror=1
	    ;;
    esac
done

exit $globalerror
