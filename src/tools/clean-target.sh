#!/bin/sh

usage() {
    echo "Usage: `basename $0` [-l] dir [dir1 dir2 ...]"
    echo "  -h   This help"
    echo "  -l   Clean out the C runtime as well"
    echo ""
    echo "Cleans out all Lisp fasls from the given directories"
    echo "If -l is also given, the C runtime is cleared as well.  This includes"
    echo "all object files, the lisp binary itself, and any selected configuration"
    echo "files.  The motif server is also removed."
    exit 1
}

while getopts "h?l" arg
do
    case $arg in
	l) CLEAN_C=1 ;;
	h | \?) usage; exit 1 ;;
    esac
done
	
shift `expr $OPTIND - 1`

if [ $# -lt 1 ]; then
    usage
fi

for d in "$@" 
do
    if [ ! -d "$d" ]; then
	echo "$d isn't a directory"
	exit 2
    fi
    D="`echo $d | sed 's:/*$::'`"
    TARGET="$TARGET $D"
done

find $TARGET -name "*.bytef" -o -name "*.lbytef" -o -name "*.assem" -o \
	-name "*.axpf" -o \
	-name "*.hpf" -o \
	-name "*.pmaxf" -o \
	-name "*.sgif" -o \
	-name "*.ppcf" -o \
	-name "*.sparcf" -o \
	-name "*.x86f" -o \
	-name "*.core" | xargs rm 2> /dev/null

for d in $TARGET
do
    rm -f $d/compile-*.log $d/hemlock/spell-dictionary.bin 2> /dev/null
    if [ -n "$CLEAN_C" ]; then
	rm -f $d/lisp/* $d/motif/server/*
    fi
done


true
