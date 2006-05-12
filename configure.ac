# configure.ac -- Autoconf configuration for the MPS
# $Id$
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_REVISION([$Id$])
AC_COPYRIGHT([Copyright 2006 Ravenbrook Limited])

AC_INIT([Memory Pool System], [1.106.2], [mps-bugs@ravenbrook.com], [mps])
AC_CONFIG_AUX_DIR([config])
AC_CONFIG_SRCDIR([code/mpm.c])

AC_CANONICAL_TARGET

AM_INIT_AUTOMAKE
AM_CONFIG_HEADER([code/autoconf.h])

AC_PROG_CC
AC_PROG_RANLIB
AM_PROG_AS
AC_FUNC_MMAP

case $target in
powerpc-*-darwin*) pfm="XCPPGC";;
i386-*-darwin*) pfm="XCI3GC";;
# etc.
*) echo "Unknown target!" 1>&2; exit 1;;
esac

AM_CONDITIONAL(XCPPGC, test "$pfm" = "XCPPGC")
AM_CONDITIONAL(XCI3GC, test "$pfm" = "XCI3GC")

AC_OUTPUT([Makefile code/Makefile])
