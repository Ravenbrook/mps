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

AC_PREREQ([2.69])
# http://www.gnu.org/software/autoconf/manual/autoconf.html#Initializing-configure
AC_INIT([Memory Pool System Kit],
        m4_esyscmd_s([sed -n '/^#define MPS_RELEASE "\(.*\)"$/{s/.*"\(.*\)"/\1/;p;}' code/version.c]),
        [mps-questions@ravenbrook.com],
        [mps-kit],
        [http://www.ravenbrook.com/project/mps/])
AC_CONFIG_AUX_DIR(tool/autoconf/build-aux)
AC_CONFIG_SRCDIR([code/mps.c])
AC_CONFIG_MACRO_DIR([tool/autoconf/build-aux])

# Automake configuration
AM_INIT_AUTOMAKE([foreign subdir-objects])

# Checks for programs.
AC_LANG_C
AC_PROG_CC([clang gcc cl cc])
AC_PROG_CC_C89

# Compiler flags autoconf extension
AX_COMPILER_FLAGS(CFLAGS)
AX_CHECK_COMPILE_FLAG([-Wno-cast-align], [CFLAGS="${CFLAGS} -Wno-cast-align"])
AX_CHECK_COMPILE_FLAG([-pedantic], [CFLAGS="${CFLAGS} -pedantic"])
AX_CHECK_COMPILE_FLAG([-Waggregate-return], [CFLAGS="${CFLAGS} -Waggregate-return"])
AX_CHECK_COMPILE_FLAG([-Wall], [CFLAGS="${CFLAGS} -Wall"])
AX_CHECK_COMPILE_FLAG([-Wcast-qual], [CFLAGS="${CFLAGS} -Wcast-qual"])
AX_CHECK_COMPILE_FLAG([-Wconversion], [CFLAGS="${CFLAGS} -Wconversion"])
AX_CHECK_COMPILE_FLAG([-Wduplicate-enum], [CFLAGS="${CFLAGS} -Wduplicate-enum"])
AX_CHECK_COMPILE_FLAG([-Werror], [CFLAGS="${CFLAGS} -Werror"])
AX_CHECK_COMPILE_FLAG([-Wextra], [CFLAGS="${CFLAGS} -Wextra"])
AX_CHECK_COMPILE_FLAG([-Winline], [CFLAGS="${CFLAGS} -Winline"])
AX_CHECK_COMPILE_FLAG([-Wmissing-prototypes], [CFLAGS="${CFLAGS} -Wmissing-prototypes"])
AX_CHECK_COMPILE_FLAG([-Wmissing-variable-declarations], [CFLAGS="${CFLAGS} -Wmissing-variable-declarations"])
AX_CHECK_COMPILE_FLAG([-Wnested-externs], [CFLAGS="${CFLAGS} -Wnested-externs"])
AX_CHECK_COMPILE_FLAG([-Wno-extended-offsetof], [CFLAGS="${CFLAGS} -Wno-extended-offsetof"])
AX_CHECK_COMPILE_FLAG([-Wpointer-arith], [CFLAGS="${CFLAGS} -Wpointer-arith"])
AX_CHECK_COMPILE_FLAG([-Wshadow], [CFLAGS="${CFLAGS} -Wshadow"])
AX_CHECK_COMPILE_FLAG([-Wstrict-aliasing=2], [CFLAGS="${CFLAGS} -Wstrict-aliasing=2"])
AX_CHECK_COMPILE_FLAG([-Wstrict-prototypes], [CFLAGS="${CFLAGS} -Wstrict-prototypes"])
AX_CHECK_COMPILE_FLAG([-Wunreachable-code], [CFLAGS="${CFLAGS} -Wunreachable-code"])
AX_CHECK_COMPILE_FLAG([-Wwrite-strings], [CFLAGS="${CFLAGS} -Wwrite-strings"])

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
