# Makefile.am -- Automake configuration for the MPS
# $Id$

# These files are part of the MPM on every platform.

MPMCOMMON = mpsi.c mpm.c arenavm.c arenacl.c arena.c global.c locus.c \
            tract.c walk.c reserv.c protocol.c pool.c poolabs.c \
            trace.c root.c seg.c format.c buffer.c ref.c bt.c ring.c \
            shield.c ld.c event.c sac.c message.c \
            poolmrg.c poolmfs.c poolmv.c dbgpool.c dbgpooli.c \
            boot.c meter.c splay.c cbs.c version.c

# These are the extra files for each supported platform, except for UNKNOWN
# which are the ANSI-based simulations for an unknown platform.

MPMPF_XCPPGC = lockan.c than.c vmxc.c protxc.c protxcpp.c prmcan.c span.c ssxcpp.s
MPMPF_UNKNOWN = lockan.c than.c vman.c protan.c prmcan.c span.c ssan.c
MPMPF_XCI3GC = $(MPMPF_UNKNOWN)


# Libraries: one for each variety

lib_LIBRARIES = libmpshi.a libmpsci.a

libmpshi_a_CFLAGS = -DCONFIG_PROD_MPS -DCONFIG_VAR_HI -DNDEBUG
libmpsci_a_CFLAGS = -DCONFIG_PROD_MPS -DCONFIG_VAR_CI


# Tests

TESTS = amcss
EXTRA_PROGRAMS = amcss

FMTDYTST = fmtdy.c fmtno.c fmtdytst.c
AMC = poolamc.c
PLINTH = mpsliban.c mpsioan.c
TESTLIB = testlib.c

amcss_SOURCES = $(FMTDYTST) $(PLINTH) $(AMC) $(TESTLIB)
amcss_CFLAGS = -DCONFIG_PROD_MPS -DCONFIG_VAR_CI
amcss_LDADD = libmpsci.a


# Sources for each variety.
# Each block is identical to the first block, but with the variety changed.
# Automake is too stupid to allow me to write this with a loop.
# GNU make is also stupid this way.  FreeBSD make is the best at this.

if XCPPGC
libmpshi_a_SOURCES = $(MPMCOMMON) $(MPM_PF_XCPPGC)
else
if XCI3GC
libmpshi_a_SOURCES = $(MPMCOMMON) $(MPM_PF_XCI3GC)
else
libmpshi_a_SOURCES = $(MPMCOMMON) $(MPM_PF_UNKNOWN)
endif
endif

if XCPPGC
libmpsci_a_SOURCES = $(MPMCOMMON) $(MPM_PF_XCPPGC)
else
if XCI3GC
libmpsci_a_SOURCES = $(MPMCOMMON) $(MPM_PF_XCI3GC)
else
libmpsci_a_SOURCES = $(MPMCOMMON) $(MPM_PF_UNKNOWN)
endif
endif
