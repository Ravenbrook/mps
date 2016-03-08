lib_LIBRARIES = lib/libmps.a
lib_libmps_a_SOURCES = code/mps.c

noinst_LIBRARIES = lib/libtest.a
lib_libtest_a_SOURCES = code/testlib.c code/fmtdy.c code/fmtdytst.c code/fmtno.c

bin_PROGRAMS = bin/amcss
bin_amcss_SOURCES = code/amcss.c
bin_amcss_LDADD = lib/libtest.a lib/libmps.a -lpthread -lm

TESTS = bin/amcss

# The fine Automake manual talks about how "tests *will* be run" but
# fails to tell you how to run them.  It turns out you have to "make
# check", but most things, like Travis CI, expect "make test".
.PHONY: test
test: check
