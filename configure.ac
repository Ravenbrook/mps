# configure.ac -- autoconf configuration for the MPS    -*- Autoconf -*-
#
# $Id$
# Copyright (C) 2012-2014 Ravenbrook Limited.  See end of file for license.
#
# YOU DON'T NEED AUTOCONF TO BUILD THE MPS
# This is just here for people who want or expect a configure script.
# See [Building the Memory Pool System](manual/build.txt) for how best
# to build and integrate the MPS.
#
# Generate the configure script with
#
#     autoreconf -vif
#

AC_PREREQ([2.50])
# http://www.gnu.org/software/autoconf/manual/autoconf.html#Initializing-configure
AC_INIT([Memory Pool System Kit],
        m4_esyscmd_s([sed -n '/^#define MPS_RELEASE "\(.*\)"$/{s/.*"\(.*\)"/\1/;p;}' code/version.c]),
        [mps-questions@ravenbrook.com],
        [mps-kit],
        [http://www.ravenbrook.com/project/mps/])
AC_CONFIG_AUX_DIR(tool/autoconf/build-aux)
AC_CONFIG_SRCDIR([code/mps.c])

# Automake configuration
AM_INIT_AUTOMAKE([foreign subdir-objects])

# Checks for programs.
AC_LANG_C
AC_PROG_CC([clang gcc cl cc])
AC_PROG_CC_C89

AC_PROG_RANLIB

# Check for Clang/LLVM.
AC_CHECK_DECL([__clang__],[CLANG=yes],[CLANG=no])

# These flags aren't used for building the MPS, but for sample programs.
CFLAGS_GC="-ansi -pedantic -Wall -Werror -Wpointer-arith \
           -Wstrict-prototypes -Wmissing-prototypes \
           -Winline -Waggregate-return -Wnested-externs \
           -Wcast-qual -Wstrict-aliasing=2 -O -g3 -pthread"
CFLAGS_LL="$CFLAGS_GC -Wno-extended-offsetof"

AC_CONFIG_FILES(Makefile example/scheme/Makefile)

AC_OUTPUT

echo 1>&2 "CONFIGURE/MAKE IS NOT THE BEST WAY TO BUILD THE MPS -- see <manual/build.txt>"
