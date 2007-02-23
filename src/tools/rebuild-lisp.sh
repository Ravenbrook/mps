#!/bin/sh

if [ "$1" = "" ]
then
	echo "Usage: $0 target-directory"
	exit 1
fi

if [ ! -d "$1" ]
then
	echo "$1 isn't a directory"
	exit 2
fi

TARGET="`echo $1 | sed 's:/*$::'`"

# Find GNU make:

if [ "$MAKE" = "" ]
then	
	MAKE="`which gmake`" || MAKE="`which make`"
fi

export MAKE

${MAKE} -C $TARGET/lisp clean && ${MAKE} -C $TARGET/lisp
