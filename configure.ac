# configure.ac -- Autoconf configuration for the MPS
# $Id$
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_REVISION([$Id$])
AC_COPYRIGHT([Copyright 2006 Ravenbrook Limited])

AC_INIT([Memory Pool System], [1.106.2], [mps-bugs@ravenbrook.com], [mps])
AC_CONFIG_AUX_DIR([config])
AC_CONFIG_SRCDIR([code/mpm.c])

AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET

AC_DEFINE_UNQUOTED(BUILD, "${build}", build)
AC_DEFINE_UNQUOTED(HOST, "${host}", host)
AC_DEFINE_UNQUOTED(TARGET, "${target}", target)
AC_DEFINE_UNQUOTED(BUILD_ALIAS, "${build_alias}", build_alias)
AC_DEFINE_UNQUOTED(HOST_ALIAS, "${host_alias}", host_alias)
AC_DEFINE_UNQUOTED(TARGET_ALIAS, "${TARGET_ALIAS}", target_alias)
AC_DEFINE_UNQUOTED(BUILD_CPU, "${build_cpu}", build_cpu)
AC_DEFINE_UNQUOTED(HOST_CPU, "${host_cpu}", host_cpu)
AC_DEFINE_UNQUOTED(TARGET_CPU, "${target_cpu}", target_cpu)
AC_DEFINE_UNQUOTED(BUILD_VENDOR, "${build_vendor}", build_vendor)
AC_DEFINE_UNQUOTED(HOST_VENDOR, "${host_vendor}", host_vendor)
AC_DEFINE_UNQUOTED(TARGET_VENDOR, "${target_vendor}", target_vendor)
AC_DEFINE_UNQUOTED(BUILD_OS, "${build_os}", build_os)
AC_DEFINE_UNQUOTED(HOST_OS, "${host_os}", host_os)
AC_DEFINE_UNQUOTED(TARGET_OS,"${target_os}", target_os)


AM_INIT_AUTOMAKE
AM_CONFIG_HEADER([code/autoconf.h])

AC_PROG_CC
AC_PROG_RANLIB
AM_PROG_AS
AC_FUNC_MMAP

AC_OUTPUT([Makefile code/Makefile])
