#!/bin/sh

LISP_VARIANT=
MOTIF_VARIANT=
TARGET_DIR=

usage() {
    echo "Usage: `basename $0` target-dir [lisp-variant [motif-variant]]"
    echo ""
    echo "Creates a directory structure in TARGET-DIR for use in compiling"
    echo "CMUCL.  If the lisp-variant is not given, uname is used to select"
    echo "a version, if possible.  If motif-variant is not given, one is"
    echo "determined from the lisp-variant."
    echo ""
    # List possible values for lisp-variant and motif-variant
    echo "Possible lisp-variants:"
    ( cd src/lisp/ ; ls -1 Config.* ) | sed 's;^Config[.];;g' | \
	    pr -3at -o 8
    echo "Possible Motif-variants:"
    ( cd src/motif/server/ ; ls -1 Config.* ) | sed 's;^Config[.];;g' | \
	    pr -3at -o 8
    exit 1
}

if [ $# = 1 ]; then
    # Only target directory given.  Try to deduce the lisp-variant
    TARGET_DIR="$1"
    case `uname -s` in
    SunOS) LISP_VARIANT=sun4_solaris_gcc ;;
    Linux) LISP_VARIANT=linux_gencgc ;;
    Darwin) LISP_VARIANT=ppc_darwin ;;
    FreeBSD) LISP_VARIANT=FreeBSD_gencgc ;;
    # Please fill in some other common systems
    *) echo "Sorry, please specify the desired Lisp variant." 
       exit 1 ;;
    esac
elif [ $# = 2 ]; then
    # Target directory and lisp-variant given 
    TARGET_DIR="$1"
    LISP_VARIANT="$2"
elif [ $# = 3 ]; then
    # Target directory, lisp-variant, and motif variant given 
    TARGET_DIR="$1"
    LISP_VARIANT="$2"
    MOTIF_VARIANT="$3"
else
    usage
fi

[ -d $1 ] && echo "Error: Directory $1 exists already!" && exit 2

TARGET="`echo $TARGET_DIR | sed 's:/*$::'`"

# Make sure the given variants exist
if [ ! -f src/lisp/Config.$LISP_VARIANT ]; then
	echo "No such lisp-variant could be found: Config.$LISP_VARIANT"
	exit 1
fi

# From the given variant, try to derive a motif variant
if [ "$MOTIF_VARIANT" = "" ]; then
    case $LISP_VARIANT in
      alpha_linux) MOTIF_VARIANT=alpha_linux ;;
      alpha_osf1) MOTIF_VARIANT=alpha_osf1 ;;
      FreeBSD*) MOTIF_VARIANT=FreeBSD ;;
      NetBSD*) MOTIF_VARIANT=NetBSD ;;
      OpenBSD*) MOTIF_VARIANT=OpenBSD ;;
      *_darwin) MOTIF_VARIANT=Darwin ;;
      sun4_solaris*) MOTIF_VARIANT=solaris ;;
      sun4c*) MOTIF_VARIANT=sun4c_411 ;;
      hp700*) MOTIF_VARIANT=hpux_cc ;;
      pmax_mach) MOTIF_VARIANT=pmax_mach ;;
      sgi*) MOTIF_VARIANT=irix ;;
      linux*) MOTIF_VARIANT=x86 ;;
    esac
elif [ ! -f src/motif/server/Config.$MOTIF_VARIANT ]; then
    echo "No such motif-variant could be found: Config.$MOTIF_VARIANT"
    exit 1
fi

# Ravenbrook: hack out the motif building.
MOTIF_VARIANT=""

# Tell user what's we've configured
echo "Lisp = $LISP_VARIANT"
echo "Motif = $MOTIF_VARIANT"

# Create a directory tree that mirrors the source directory tree
find src -name 'CVS' -prune -o -type d -print \
	| sed "s:^src:$TARGET:g" | xargs mkdir

# Link Makefile and Config files
( cd $TARGET/lisp ; ln -s ../../src/lisp/GNUmakefile ./Makefile )
( cd $TARGET/lisp ; ln -s ../../src/lisp/Config.$LISP_VARIANT ./Config )

# Create empty initial map file
echo 'Map file for lisp version 0' > $TARGET/lisp/lisp.nm

# Create dummy internals.h so we get warned to recompile
echo '#error You need to run genesis (via build-world.sh) before compiling the startup code!' > $TARGET/lisp/internals.h

SETENV=src/tools/setenv-scripts

# Create sample setenv.lisp file
cat $SETENV/base-features.lisp > $TARGET/setenv.lisp

# Put in some platform specific items
case $LISP_VARIANT in
  *linux*)
      case $$LISP_VARIANT in
        *_gencgc*) gcname=":gencgc" ;;
	*) gcname=":cgc" ;;
      esac
      sed "s;@@gcname@@;$gcname;" $SETENV/linux-features.lisp >> $TARGET/setenv.lisp
      ;;
  *OpenBSD*)
      case $LISP_VARIANT in
        *_gencgc*) gcname=":gencgc" ;;
	*) gcname=":cgc" ;;
      esac
      sed "s;@@gcname@@;$gcname;" $SETENV/openbsd-features.lisp >> $TARGET/setenv.lisp
      ;;
  *FreeBSD*)
      case $LISP_VARIANT in
        *_gencgc*) gcname=":gencgc" ;;
	*) gcname=":cgc" ;;
      esac
      sed "s;@@gcname@@;$gcname;" $SETENV/freebsd-features.lisp >> $TARGET/setenv.lisp
      ;;
  *solaris*)
      cat $SETENV/solaris-features.lisp >> $TARGET/setenv.lisp
      ;;
  *)
      sed "s;@@LISP@@;$LISP_VARIANT;" $SETENV/unknown.lisp >> $TARGET/setenv.lisp
      ;;
esac


# Do Motif setup
if [ "$MOTIF_VARIANT" != "" ]
then
    ( cd $TARGET/motif/server ; ln -s ../../../src/motif/server/GNUmakefile ./Makefile )
    ( cd $TARGET/motif/server ; ln -s ../../../src/motif/server/Config.$MOTIF_VARIANT ./Config )
fi
