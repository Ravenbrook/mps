#!/bin/sh

while getopts "G:O:bgh?" arg
do
    case $arg in
	G) GROUP="-g $OPTARG" ;;
	O) OWNER="-o $OPTARG" ;;
	b) ENABLE_BZIP=-b ;;
	g) ENABLE_GZIP=-g  ;;
	h | \?) usage; exit 1 ;;
    esac
done

shift `expr $OPTIND - 1`

if [ "$1" = "" -o "$2" = "" -o "$3" = "" -o "$4" = "" ]
then
	echo "Usage: $0 target-directory version arch os"
	exit 1
fi

if [ ! -d "$1" ]
then
	echo "$1 isn't a directory"
	exit 2
fi

DESTDIR=release
TARGET="`echo $1 | sed 's:/*$::'`"
VERSION=$2
ARCH=$3
OS=$4

case $ARCH in
	x86*)		FASL=x86f ;;
	sparc*)		FASL=sparcf ;;
	alpha*)		FASL=axpf ;;
	ppc*)		FASL=ppcf ;;
	mips*)		FASL=sgif ;;
	*)
		echo "Unknown FASL type for architecture $ARCH"
		exit 2
		;;
esac

# Frob PATH to use /usr/ucb/install for Solaris
if [ `uname -s` = "SunOS" ]
then
	PATH=/usr/ucb:$PATH
fi

echo Cleaning $DESTDIR
[ -d $DESTDIR ] && rm -rf $DESTDIR

echo Installing main components
install -d ${GROUP} ${OWNER} -m 0755 $DESTDIR/bin
install -d ${GROUP} ${OWNER} -m 0755 $DESTDIR/doc/cmucl
install -d ${GROUP} ${OWNER} -m 0755 $DESTDIR/lib/cmucl
install -d ${GROUP} ${OWNER} -m 0755 $DESTDIR/lib/cmucl/lib
install -d ${GROUP} ${OWNER} -m 0755 $DESTDIR/lib/cmucl/lib/subsystems
install -d ${GROUP} ${OWNER} -m 0755 $DESTDIR/man/man1
install ${GROUP} ${OWNER} -m 0755 $TARGET/lisp/lisp $DESTDIR/bin/
install ${GROUP} ${OWNER} -m 0644 $TARGET/lisp/lisp.core $DESTDIR/lib/cmucl/lib/
install ${GROUP} ${OWNER} -m 0755 src/tools/load-foreign.csh src/tools/config \
	$DESTDIR/lib/cmucl/lib/
install ${GROUP} ${OWNER} -m 0644 src/tools/config.lisp \
	$DESTDIR/lib/cmucl/lib/
install ${GROUP} ${OWNER} -m 0644 src/code/generic-site.lisp \
	$DESTDIR/lib/cmucl/lib/
install ${GROUP} ${OWNER} -m 0644 $TARGET/lisp/lisp.nm $TARGET/lisp/lisp.map \
	$TARGET/lisp/internals.h $TARGET/lisp/internals.inc $DESTDIR/lib/cmucl/
install ${GROUP} ${OWNER} -m 0755 src/tools/sample-wrapper $DESTDIR/lib/cmucl/

for f in gray-streams gray-compat simple-streams iodefs
do
    install ${GROUP} ${OWNER} -m 0644 $TARGET/pcl/$f-library.$FASL $DESTDIR/lib/cmucl/lib/subsystems/
done

install ${GROUP} ${OWNER} -m 0644 src/general-info/cmucl.1 \
	$DESTDIR/man/man1/
install ${GROUP} ${OWNER} -m 0644 src/general-info/lisp.1 \
	$DESTDIR/man/man1/
install ${GROUP} ${OWNER} -m 0644 src/general-info/README $DESTDIR/doc/cmucl/
if [ -f src/general-info/release-$VERSION.txt ] 
then
	install ${GROUP} ${OWNER} -m 0644 src/general-info/release-$VERSION.txt \
		$DESTDIR/doc/cmucl/
fi

sync ; sleep 1 ; sync ; sleep 1 ; sync
echo Tarring main components
if [ -n "$ENABLE_GZIP" ]; then
    echo "  Compressing with gzip"
    ( cd $DESTDIR >/dev/null ; tar cf - bin doc lib man ) | \
	gzip -c > cmucl-$VERSION-$ARCH-$OS.tar.gz
fi
if [ -n "$ENABLE_BZIP" ]; then
    echo "  Compressing with bzip"
    ( cd $DESTDIR >/dev/null ; tar cf - bin doc lib man ) | \
	bzip2 > cmucl-$VERSION-$ARCH-$OS.tar.bz2
fi

echo Cleaning $DESTDIR
[ -d $DESTDIR ] && rm -rf $DESTDIR
echo Done
