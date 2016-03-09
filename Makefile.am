bin_PROGRAMS =

lib_LIBRARIES = lib/libmps.a
lib_libmps_a_SOURCES = code/mps.c
lib_libmps_a_CFLAGS = @MPS_CFLAGS@

noinst_LIBRARIES = lib/libtest.a
lib_libtest_a_SOURCES = code/testlib.c code/fmtdy.c code/fmtdytst.c code/fmtno.c
lib_libtest_a_CFLAGS = @MPS_CFLAGS@

bin_PROGRAMS += bin/amcss
bin_amcss_SOURCES = code/amcss.c
bin_amcss_LDADD = lib/libtest.a lib/libmps.a -lpthread -lm
lib_amcss_CFLAGS = @MPS_CFLAGS@

bin_PROGRAMS += bin/scheme
bin_scheme_SOURCES = example/scheme/scheme.c
bin_scheme_LDADD = lib/libmps.a -lpthread -lm
bin_scheme_CFLAGS = -I code -O -g3 @SCHEME_CFLAGS@

TESTS = bin/amcss

# The fine Automake manual talks about how "tests *will* be run" but
# fails to tell you how to run them.  It turns out you have to "make
# check", but most things, like Travis CI, expect "make test".
.PHONY: test
test: check
