#!/bin/sh

usage() {
    echo "make-dist.sh: [-hbg] [-G group] [-O owner] dir version arch os"
    echo "  -h           This help"
    echo "  -b           Use bzip2 compression"
    echo "  -g           Use gzip compression"
    echo "  -G group     Group to use"
    echo "  -O owner     Owner to use"
    echo "   dir         Directory where the build is located"
    echo "   version     Version (usually date and/or other version info)"
    echo "   arch        Architecture (x86, sparc, etc.)"
    echo "   os          OS (linux, solaris8, etc.)"
    echo ""
    echo "Make a CMUCL distribution consisting of two tar files.  One holds"
    echo "the main files including the C runtime, the lisp core, and PCL library."
    echo "The second tar file contains extra libraries such as CLX, CLM, and"
    echo "Hemlock."
    echo ""
    echo "The tar files have the form cmucl-<version>-<arch>-<os>.tar.<c>"
    echo "and cmucl-<version>-<arch>-<os>.extra.tar.<c> where <version>,"
    echo "<arch>, and <os> are given values, and <c> is gz or bz2 depending"
    echo "on the selected compression method."
    exit 1
}

while getopts "G:O:bgh?" arg
do
    case $arg in
	G) GROUP=$OPTARG ;;
	O) OWNER=$OPTARG ;;
	b) ENABLE_BZIP=-b ;;
	g) ENABLE_GZIP=-g  ;;
	h | \?) usage; exit 1 ;;
    esac
done
	
shift `expr $OPTIND - 1`

# Figure out the architecture and OS

if [ $# -lt 2 ]; then
    usage
else
    # Figure out the architecture and OS
    ARCH=
    OS=
    case `uname -s` in
      SunOS)
	  ARCH=sparcv9
	  OS=solaris8
	  ;;
      Linux)
	  ARCH=x86
	  OS=linux
	  ;;
      Darwin)
	  ARCH=ppc
	  OS=darwin
	  ;;
      esac

      if [ $# -eq 3 ]; then
	  ARCH=$3
      elif [ $# -eq 4 ]; then
	  ARCH=$3
	  OS=$4
      fi
fi

if [ ! -d "$1" ]
then
	echo "$1 isn't a directory"
	exit 2
fi

if [ -z "$ARCH" ]; then
    echo "Unknown architecture.  Please specify one"
    usage
fi

if [ -z "$OS" ]; then
    echo "Unknown OS.  Please specify one"
    usage
fi
    
  
TARGET="`echo $1 | sed 's:/*$::'`"
VERSION=$2

ROOT=`dirname $0`

# If no compression options given, default to gzip
if [ -z "$ENABLE_GZIP" -a -z "$ENABLE_BZIP" ]; then
    ENABLE_GZIP="-g"
fi

OPTIONS="${GROUP:+ -G ${GROUP}} ${OWNER:+ -O ${OWNER}} $ENABLE_GZIP $ENABLE_BZIP"

echo Creating distribution for $ARCH $OS
$ROOT/make-main-dist.sh $OPTIONS $TARGET $VERSION $ARCH $OS || exit 1
$ROOT/make-extra-dist.sh $OPTIONS $TARGET $VERSION $ARCH $OS || exit 2
