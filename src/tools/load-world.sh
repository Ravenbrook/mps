#!/bin/sh

if [ "$1" = "" -o "$2" = "" ]
then
	echo "Usage: $0 target-directory version"
	exit 1
fi

if [ ! -d "$1" ]
then
	echo "$1 isn't a directory"
	exit 2
fi

TARGET="`echo $1 | sed 's:/*$::'`"

$TARGET/lisp/lisp -core $TARGET/lisp/kernel.core <<EOF
(in-package :cl-user)

(setf (ext:search-list "target:")
      '("$TARGET/" "src/"))

(load "target:setenv")

(pushnew :no-clx *features*)
(pushnew :no-clm *features*)
(pushnew :no-hemlock *features*)

(load "target:tools/worldload")
$2

EOF
