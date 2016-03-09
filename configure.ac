# configure.ac -- autoconf configuration for the MPS    -*- Autoconf -*-
#
# $Id$
# Copyright (C) 2012-2014 Ravenbrook Limited.  See end of file for license.
#
# YOU DON'T NEED AUTOCONF TO BUILD THE MPS
#
# Automake and Autoconf are here for developers of the MPS itself.
# See "Building the Memory Pool System" (manual/build.txt) for how
# best to build and integrate the MPS into your application.
#
# Generate the configure script with
#
#     autoreconf -vif
#

AC_PREREQ([2.64])
# http://www.gnu.org/software/autoconf/manual/autoconf.html#Initializing-configure
AC_INIT([Memory Pool System Kit],
        m4_esyscmd_s([sed -n '/^#define MPS_RELEASE "\(.*\)"$/{s/.*"\(.*\)"/\1/;p;}' code/version.c]),
        [mps-questions@ravenbrook.com],
        [mps-kit],
        [http://www.ravenbrook.com/project/mps/])
AC_CONFIG_AUX_DIR(tool/autoconf/build-aux)
AC_CONFIG_SRCDIR([code/mps.c])
AC_CONFIG_MACRO_DIR([tool/autoconf/macro])

# Automake configuration
AM_INIT_AUTOMAKE([foreign subdir-objects])
# Don't rerun automake by default.  Allows Travis CI etc. to build
# even without a compatible version of automake.
AM_MAINTAINER_MODE([disable])

# Checks for programs.
AC_LANG_C
AC_PROG_CC([clang gcc cl cc])
AC_PROG_CC_C89
AC_PROG_RANLIB

echo "CC=$CC"

AX_APPEND_COMPILE_FLAGS([ dnl
	-std=c89 dnl
	-pedantic dnl
	-Wall dnl
	-Werror dnl
	-Wpointer-arith dnl
	-Wstrict-prototypes dnl
	-Wmissing-prototypes dnl
	-Winline dnl
	-Waggregate-return dnl
	-Wnested-externs dnl
	-Wcast-qual dnl
	-Wstrict-aliasing=2], SCHEME_CFLAGS)

echo "SCHEME_CFLAGS=$SCHEME_CFLAGS"

case $CC in
clang)	AX_APPEND_COMPILE_FLAGS([ dnl
                -std=c89 dnl
                -pedantic dnl
                -Waggregate-return dnl
                -Wall dnl
                -Wcast-qual dnl
                -Wconversion dnl
                -Wduplicate-enum dnl
                -Werror dnl
                -Wextra dnl
                -Winline dnl
                -Wmissing-prototypes dnl
                -Wmissing-variable-declarations dnl
                -Wnested-externs dnl
                -Wno-extended-offsetof dnl
                -Wpointer-arith dnl
                -Wshadow dnl
                -Wstrict-aliasing=2 dnl
                -Wstrict-prototypes dnl
                -Wunreachable-code dnl
                -Wwrite-strings], MPS_CFLAGS)
        AX_APPEND_COMPILE_FLAGS([-Wno-extended-offsetof], SCHEME_CFLAGS)
	;;
gcc)
	AX_APPEND_COMPILE_FLAGS([ dnl
                -std=c89 dnl
                -pedantic dnl
		-Waggregate-return dnl
		-Wall dnl
		-Wcast-qual dnl
		-Werror dnl
		-Wextra dnl
		-Winline dnl
		-Wmissing-prototypes dnl
		-Wnested-externs dnl
		-Wpointer-arith dnl
		-Wshadow dnl
		-Wstrict-aliasing=2 dnl
		-Wstrict-prototypes dnl
		-Wswitch-default dnl
		-Wwrite-strings], MPS_CFLAGS)
	;;
esac

# Compiler flags autoconf extension
#AX_COMPILER_FLAGS(MPS_CFLAGS)
#AX_APPEND_COMPILE_FLAGS([-Wno-format-nonliteral -Wno-missing-noreturn], SCHEME_CFLAGS)

AC_SUBST(MPS_CFLAGS)
AC_SUBST(SCHEME_CFLAGS)

AC_CONFIG_FILES(Makefile example/scheme/Makefile)

AC_OUTPUT

echo 1>&2 "CONFIGURE/MAKE IS NOT THE BEST WAY TO BUILD THE MPS -- see <manual/build.txt>"
