lib_LIBRARIES = libmps.a
libmps_a_SOURCES = code/mps.c

noinst_LIBRARIES = testlib.a
testlib_a_SOURCES = code/testlib.c code/fmtdy.c code/fmtdytst.c code/fmtno.c

bin_PROGRAMS = amcss
amcss_SOURCES = code/amcss.c
amcss_LDADD = testlib.a libmps.a -lpthread -lm
