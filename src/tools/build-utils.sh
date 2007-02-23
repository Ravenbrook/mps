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

$TARGET/lisp/lisp -core $TARGET/lisp/lisp.core \
	-noinit -nositeinit -batch <<EOF || exit 3
(in-package :cl-user)

(setf lisp::*enable-package-locked-errors* nil)
(setf (ext:search-list "target:")
      '("$TARGET/" "src/"))

(load "target:setenv")

(pushnew :no-clx *features*)
(pushnew :no-clm *features*)
(pushnew :no-hemlock *features*)

(compile-file "target:tools/setup" :load t)
(setq *gc-verbose* nil *interactive* nil)
(load "target:tools/clxcom")
(load "target:clx/clx-library")
(load "target:tools/clmcom")
(load "target:tools/hemcom")

EOF

# Find GNU make:

if [ "$MAKE" = "" ]
then    
        MAKE="`which gmake`" || MAKE="`which make`"
fi

export MAKE

${MAKE} -C $TARGET/motif/server clean && ${MAKE} -C $TARGET/motif/server
