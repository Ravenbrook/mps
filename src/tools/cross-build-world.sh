#!/bin/sh

if [ "$1" = "" -o "$2" = "" ]
then
        echo "Usage: $0 target-directory cross-directory cross-compiler-script [build-binary] [build-flags...]"
        exit 1
fi

if [ ! -d "$1" ]
then
        echo "$1 isn't a directory"
        exit 2
fi

if [ -f "$2" ]
then
	echo "$2 exists but isn't a directory"
	exit 2
fi

if [ ! -f "$3" ]
then
	echo "$3 doesn't exist, or isn't a normal file"
fi

TARGET="`echo $1 | sed 's:/*$::'`"
CROSS="`echo $2 | sed 's:/*$::'`"
SCRIPT="$3"
LISP="${4:-lisp}"
if [ $# -ge 4 ]
then
	shift 4
else
	shift 3
fi

if [ ! -d "$CROSS" ]
then
	# Create a directory tree that mirrors the source directory tree
	find src -name 'CVS' -prune -o -type d -print | \
		sed "s:^src:$CROSS:g" | xargs mkdir
fi

$LISP "$@" -noinit -nositeinit <<EOF
(in-package :cl-user)

(setf lisp::*enable-package-locked-errors* nil)

(setf (ext:search-list "target:")
      '("$CROSS/" "src/"))

(when (probe-file "$TARGET/cross-bootstrap.lisp")
  (load "$TARGET/cross-bootstrap.lisp"))

(load "target:code/exports")
(load "target:tools/setup" :if-source-newer :load-source)
(comf "target:tools/setup" :load t)

(setq *gc-verbose* nil *interactive* nil)

(load "$SCRIPT")

(pushnew :bootstrap *features*)

(setf (ext:search-list "target:")
      '("$TARGET/" "src/"))

(when (probe-file "target:bootstrap.lisp")
  (load "target:bootstrap.lisp"))

(load "target:setenv")

(pushnew :no-pcl *features*)
(pushnew :no-clx *features*)
(pushnew :no-clm *features*)
(pushnew :no-hemlock *features*)

(load "target:tools/worldcom")
#-(or no-compiler runtime) (load "target:tools/comcom")
;; Compile at least new-genesis, so that genesis doesn't take ages
#+(or no-compiler runtime) (comf "target:compiler/generic/new-genesis")
#-(or no-pcl runtime) (load "target:tools/pclcom")

(setq *gc-verbose* t *interactive* t)

(load "target:tools/worldbuild")
(ext:quit)
EOF
