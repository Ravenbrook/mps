# Makefile.am -- Automake configuration for the MPS
# $Id$

# Don't require GNU-style AUTHORS, ChangeLog, etc.
# AUTOMAKE_OPTIONS = foreign

lib_LIBRARIES = libmps.a
libmps_a_SOURCES = \
        mpsi.c mpm.c arenavm.c arenacl.c arena.c global.c locus.c \
        tract.c walk.c reserv.c protocol.c pool.c poolabs.c \
        trace.c root.c seg.c format.c buffer.c ref.c bt.c ring.c \
        shield.c ld.c event.c sac.c message.c \
        poolmrg.c poolmfs.c poolmv.c dbgpool.c dbgpooli.c \
        boot.c meter.c splay.c cbs.c version.c \
        lockan.c than.c vmxc.c protxc.c protxcpp.c prmcan.c span.c \
        ssxcpp.s
libmps_a_CFLAGS = -DCONFIG_PROD_MPS -DCONFIG_VAR_HI -DNDEBUG
