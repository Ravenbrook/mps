# Makefile -- make rules for sc
#
# Richard Brooksby, 2004-07-28
#
# 2004-07-28  RB  Added comment and Perforce ID keyword.  Added rules for
# "hello" program.
#
# 2004-07-29  RB  Added cross reference to Apple documentation on bundles.
#
# 2004-08-04  RB  Breaking sc.c down into separate units.
#
# 2004-08-09  RB  Adding "init" unit into SC library, with separate "SC
# nucleus" library used to build it.  Adding compiler integrity check.
#
# $Id$

CFLAGS = -ansi -std=iso9899:1990 -pedantic -Wall -Wpointer-arith \
	 -Wstrict-prototypes -Wmissing-prototypes \
	 -Winline -Waggregate-return -Wnested-externs \
	 -Wcast-qual -Wshadow \
	 -W -Wfloat-equal -Wundef -Wshadow \
	 -Wpointer-arith \
         -Wbad-function-cast -Wcast-qual -Wcast-align \
 	 -Wwrite-strings \
 	 -Wsign-compare -Waggregate-return \
	 -Wstrict-prototypes -Wmissing-prototypes -Wmissing-declarations \
	 -Wmissing-noreturn -Wmissing-format-attribute \
	 -Wno-deprecated-declarations -Wpacked \
	 -Wnested-externs -Wunreachable-code -Winline -Wno-long-double \
	 -Wlong-long -Wdisabled-optimization \
	 -Wno-unused-label \
	 -g -ggdb3 -O2

#  	 -Wconversion 
#	 -Wpadded
#        -Wredundant-decls

SC_OBJS = sc.o scint.o scchar.o scport.o scrw.o screp.o \
          sceval.o sclist.o scstring.o scvector.o scsymbol.o \
          scsyntax.o scload.o scexception.o scenv.o scproc.o \
          scmm.o scstatic.o scrun.o \
          dynload_macosx.o

SCN_OBJS = $(SC_OBJS) scinitstub.o

SCL_OBJS = $(SC_OBJS) scinit.o

sc: main.o libsc.a
	@echo "$@ <- $^"
	@$(CC) -o $@ $(CFLAGS) $^ -L. -lsc

scn: main.o libscn.a
	@echo "$@ <- $^"
	@$(CC) -o $@ $(CFLAGS) $^ -L. -lscn

libscn.a: $(SCN_OBJS)
	libtool -static -o $@ $^

libsc.a: $(SCL_OBJS)
	libtool -static -o $@ $^

sc.dylib: $(SC_OBJS)
	libtool -dynamic -lcc_dynamic -framework System -o $@ $^

$(SC_OBJS): sc.h

.PHONY: clean
clean:
	rm -f sc factorial factorial.c hello hello.c *.o *.so

.PHONY: test
test: sc test.sc
	echo '(load "test.sc")' | ./sc

.PHONY: compiled-test
compiled-test: sc compiled-test.so
	echo '(dynload "compiled-test.so")' | ./sc

# Check that "init.c" compiled using the full SC is the same as "scinit.c"
# compiled using the SC nucleus plus interpreted "init.sc".  If they aren't
# then the compiler isn't correct.
.PHONY: sound-init
sound-init: scinit.c init.c
	diff scinit.c init.c

%.o: %.c
	@echo "$@ <- $<"
	@$(CC) -o $@ $(CFLAGS) -c $<

# Dynamically loadable units are Mach-O "bundles" [Apple 2003-08-07, p14].
# The "-bundle_loader" argument tells the linker which executable they're
# going to be loaded into, so that the bundle can reference symbols from
# it.

%.so: %.c sc
	@echo "$@ <- $<"
	@$(CC) -o $@ -bundle -bundle_loader sc $(CFLAGS) -DSC_SHARED $<

# Special rule for compiling the initial definitions using the SC nucleus
# and intepreting the compiler.
scinit.c: init.sc scn compile.sc define-structure.sc syntax.sc expand.sc
	echo '(load "init.sc") (load "compile.sc") (with-output-to-file "$@" (lambda () (compile-file "$<")))' | time ./scn

# Prevent deletion of generated intermediate C files.
.SECONDARY: $(patsubst %.sc,%.c,$(wildcard *.sc))

%.c: %.sc sc compile.sc define-structure.sc syntax.sc expand.sc
	echo '(load "compile.sc") (with-output-to-file "$@" (lambda () (compile-file "$<")))' | time ./sc

factorial: factorial.o opcodes.h sc.h stub.o $(SC_OBJS)
	@echo "$@ <- $^"
	@$(CC) -o $@ $(CFLAGS) $< $(SC_OBJS) stub.o

hello: hello.o opcodes.h sc.h stub.o $(SC_OBJS)
	@echo "$@ <- $^"
	@$(CC) -o $@ $(CFLAGS) $< $(SC_OBJS) stub.o


# Rule for making assembler files from C files.

%.s: %.c
	@echo "$@ <- $<"
	@$(CC) -o $@ $(CFLAGS) -S $<

# Rule for making preprocessed files from C files.

%.i: %.c
	@echo "$@ <- $<"
	@$(CC) -o $@ $(CFLAGS) -E $<


# A. REFERENCES
#
# [Apple 2003-08-07] "Mach-O Runtime Architecture"; Apple Computer, Inc.;
# 2003-08-07;
# <http://developer.apple.com/documentation/DeveloperTools/Conceptual/MachORuntime/>.
