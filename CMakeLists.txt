# CMakeLists.txt -- CMake configuration for the MPS
#
# This is just an experimental demonstration configuration to see how
# we might use CMake to replace GNU Make, NMAKE, and Xcodebuild in the
# MPS.
#
# A complete review of all the GNU Make, NMAKE, etc. files and their
# usage would be required.
#
# Invoke like::
#
#   cmake . -B hot -DCMAKE_BUILD_TYPE=RELEASE --log-level=VERBOSE && cmake --build hot --verbose
#
# or like::
#
#   cmake . -B cool -DCMAKE_BUILD_TYPE=DEBUG
#   make -C cool all test
#
# or like (on Windows)::
#
#   cmake . -B cool -DCMAKE_BUILD_TYPE=DEBUG -G "NMake Makefiles"
#   cd cool
#   nmake all test
#
# See GitHub issue #146 <https://github.com/Ravenbrook/mps/issues/146>
# for discussion.
#
# TODO: Incorporate %% instructions

# Specify CMake version
#
# The latest feature use is try_compile, which requires CMake 3.25
# <https://cmake.org/cmake/help/latest/command/try_compile.html#try-compiling-source-files>.
cmake_minimum_required(VERSION 3.25)

project(
  mps
  LANGUAGES C
  DESCRIPTION "Memory Pool System" # New in CMake 3.9
  HOMEPAGE_URL "https://www.ravenbrook.com/project/mps" # New in CMake 3.12
)

# Some people say this defaults to "Release", but it's sometimes
# blank.  This file relies on it being set.
if (CMAKE_BUILD_TYPE)
  message(VERBOSE "Build type ${CMAKE_BUILD_TYPE}")
else ()
  set(CMAKE_BUILD_TYPE "RELEASE")
  message(VERBOSE "Set build type to ${CMAKE_BUILD_TYPE}")
endif ()


# Detect target platform
#
# This is a nasty hack.  CMake doesn't provide a reliably way of
# determining the target platform; nothing like the target triple from
# ``cc -dumpmachine``.  Variables like CMAKE_SYSTEM_PROCESSOR are
# documented as "not guaranteed" and are therefore useless
# <https://cmake.org/cmake/help/latest/variable/CMAKE_SYSTEM_PROCESSOR.html#variable:CMAKE_SYSTEM_PROCESSOR>.
# Astounding hacks like <https://stackoverflow.com/a/12024211> seem to
# be necessary.  This is based on that, but uses try_compile rather
# than try_run, and so should work when cross-compiling
# <https://cmake.org/cmake/help/book/mastering-cmake/chapter/Cross%20Compiling%20With%20CMake.html>.

# FIXME: This creates a dependency on the layout of mpstd.h.  Document
# and link at both ends.

# FIXME: How to override detection?

# TODO: Test with cross-compilation.

file(READ code/mpstd.h FILE_MPSTD_H)
string(
  REGEX REPLACE "\n[ \t]*#define[ \t]+MPS_PF_STRING[ \t]+" "\n#error "
  FILE_MPSTDE_C "${FILE_MPSTD_H}"
)
string(APPEND FILE_MPSTDE_C "int main(void) { return 0; }\n")
try_compile( # New in CMake 3.25
  MPSTDE_RESULT
  SOURCE_FROM_VAR "mpstde.c" FILE_MPSTDE_C
  OUTPUT_VARIABLE MPSTDE_OUTPUT
)
if (MPSTDE_RESULT)
  message(FATAL_ERROR "Target detection should have failed due to #error")
endif ()
# Uncomment the following line to help debug the #error match
# message(VERBOSE "MPSTDE_OUTPUT = ${MPSTDE_OUTPUT}")
if (MPSTDE_OUTPUT MATCHES "#error:?[ \t]+\"((..)(..)(..))\"")
  set(MPS_PF_STRING ${CMAKE_MATCH_1})
  set(OS ${CMAKE_MATCH_2})
  set(ARCH ${CMAKE_MATCH_3})
  set(COMPILER ${CMAKE_MATCH_4})
  message(VERBOSE "MPS_PF_STRING = ${CMAKE_MATCH_1}")
  message(VERBOSE "OS = ${OS}  ARCH = ${ARCH}  COMPILER = ${COMPILER}")
else ()
  message(FATAL_ERROR "Unable to detect target platform")
endif ()


# FIXME: What about ANSI platform?  Override with PLATFORM_ANSI passed
# to CMake with ``-D``, as in mps.c.  Same for freestanding platform
# (see branch/2023-02-13/freestanding).

if (OS STREQUAL "xc")

  # Lifted from xci3ll.gmk and xci6ll.gmk
  #
  # FIXME: Untested
  #
  # FIXME: xci3ll.gmk includes this line::
  #
  #   CC = clang -arch i386

  set(MPMPF
    code/lockix.c
    code/prmc${ARCH}.c
    code/prmc${OS}.c
    code/prmcxc${ARCH}.c
    code/protix.c
    code/prot${OS}.c
    code/span.c
    code/thxc.c
    code/vmix.c
  )
  
  set(TESTLIB code/testlib.c)
  set(TESTTHR code/testthrix.c)

  # FIXME: Is this necessary on macOS?
  link_libraries(m)

elseif (OS STREQUAL "li")

  # Lifted from lii3ll.gmk and lii6ll.gmk
  set(MPMPF
    code/lockix.c
    code/prmc${ARCH}.c
    code/prmcix.c
    code/prmc${OS}${ARCH}.c
    code/protix.c
    code/protsgix.c
    code/pthrdext.c
    code/span.c
    code/thix.c
    code/vmix.c
  )

  set(TESTLIB code/testlib.c)
  set(TESTTHR code/testthrix.c)

  link_libraries(m)

elseif (OS STREQUAL "fr")

  # FIXME: Fill this in from fr*.gmk

  set(TESTLIB code/testlib.c)
  set(TESTTHR code/testthrix.c)

  link_libraries(m)

elseif (OS STREQUAL "w3")

  # Lifted from w3i6mv.nmk
  #
  # FIXME: Untested
  
  set(MPMPF
    code/lock${OS}.c
    code/mpsi${OS}.c
    code/prmc${ARCH}.c
    code/prmc${OS}.c
    code/prmc${OS}${ARCH}.c
    code/prot${OS}.c
    code/sp${OS}${ARCH}.c
    code/th${OS}.c
    code/vm${OS}.c
  )

  set(TESTLIB code/testlib.c code/getoptl.c)
  set(TESTTHR code/testthr${OS}.c)

else ()

  message(FATAL_ERROR "Unable to detect target OS")
  
endif ()


# Compilers

if (COMPILER STREQUAL "gc")

  # Lifted from variables in gc.gmk
  set(CFLAGSDEBUG -O -g3)
  set(CFLAGSOPT -O2 -g3)
  set(CFLAGSCOMPILER
    -Waggregate-return
    -Wall
    -Wcast-qual
    -Werror
    -Wextra
    -Winline
    -Wmissing-prototypes
    -Wnested-externs
    -Wpointer-arith
    -Wshadow
    -Wstrict-aliasing=2
    -Wstrict-prototypes
    -Wswitch-default
    -Wwrite-strings
  )
  set(CFLAGSCOMPILERSTRICT
    -std=c89
    -pedantic
  )
  
elseif (COMPILER STREQUAL "ll")

  # Lifted from variables in ll.gmk
  set(CFLAGSDEBUG -O0 -g3)
  set(CFLAGSOPT O2 -g3)
  set(CFLAGSCOMPILER
    -Waggregate-return
    -Wall
    -Wcast-qual
    -Wconversion
    -Wduplicate-enum
    -Werror
    -Wextra
    -Winline
    -Wmissing-prototypes
    -Wmissing-variable-declarations
    -Wnested-externs
    -Wpointer-arith
    -Wshadow
    -Wstrict-aliasing=2
    -Wstrict-prototypes
    -Wunreachable-code
    -Wwrite-strings
  )
  set(CFLAGSCOMPILERSTRICT
    -std=c89
    -pedantic
  )

elseif (COMPILER STREQUAL "mv")

  # Lifted from variables in mv.nmk.
  set(CFLAGSDEBUG /MTd /Od)
  set(CFLAGSOPT /O2 /MT)

  set(CFLAGSCOMPILER
    # from CFLAGSCOMMONPRE in commpre.nmk
    /nologo
    # FIXME: PFMDEFS?
    # FIXME: CFLAGSTARGETPRE?
    /Zi
    # from mv.nmk
    /D_CRT_SECURE_NO_WARNINGS /W4 /WX /Gs
  )
  set(CFLAGSCOMPILERSTRICT )

  # FIXME: LINKFLAGS* from mv.nmk
  # What about PDBs?
  set(CMAKE_EXE_LINKER_FLAGS /LARGEADDRESSAWARE)

  # These may be the defaults for CMake. Investigate.
  # Defaults seem to be:
  # Debug: /debug /INCREMENTAL
  # Release: /INCREMENTAL:NO
  # message(VERBOSE "Linker flags debug: ${CMAKE_EXE_LINKER_FLAGS_DEBUG}")
  # message(VERBOSE "Linker flags release: ${CMAKE_EXE_LINKER_FLAGS_RELEASE}")
  # Lifted from commpre.nmk
  # FIXME: mv.nmk specifies libcmt.lib and libcmtd.lib.  Why?
  set(CMAKE_EXE_LINKER_FLAGS_DEBUG /DEBUG)
  set(CMAKE_EXE_LINKER_FLAGS_RELEASE /RELEASE)

else ()

  message(WARNING "Unknown compiler toolchain.  Compiling without warning flags.")

endif ()


# Common definitions
#
# Lifted from comm.gmk section "Common definitions".
#
# %%PART: When adding a new part, add it here, unless it's platform-specific
# These values are defined here because they have no variation between
# platforms.

set(AMC code/poolamc.c)
set(AMS code/poolams.c)
set(AWL code/poolawl.c)
set(LO code/poollo.c)
set(SNC code/poolsnc.c)
set(POOLN code/pooln.c)
set(MV2 code/poolmv2.c)
set(MVFF code/poolmvff.c)
set(FMTDY code/fmtdy.c code/fmtno.c)
set(FMTDYTST code/fmtdy.c code/fmtno.c code/fmtdytst.c)
set(FMTHETST code/fmthe.c code/fmtdy.c code/fmtno.c code/fmtdytst.c)
set(FMTSCM code/fmtscheme.c)
set(PLINTH code/mpsliban.c code/mpsioan.c)
set(MPMCOMMON
    code/abq.c
    code/arena.c
    code/arenacl.c
    code/arenavm.c
    code/arg.c
    code/boot.c
    code/bt.c
    code/buffer.c
    code/cbs.c
    code/dbgpool.c
    code/dbgpooli.c
    code/event.c
    code/failover.c
    code/format.c
    code/freelist.c
    code/global.c
    code/land.c
    code/ld.c
    code/locus.c
    code/message.c
    code/meter.c
    code/mpm.c
    code/mpsi.c
    code/nailboard.c
    code/policy.c
    code/pool.c
    code/poolabs.c
    code/poolmfs.c
    code/poolmrg.c
    code/protocol.c
    code/range.c
    code/rangetree.c
    code/ref.c
    code/ring.c
    code/root.c
    code/sa.c
    code/sac.c
    code/scan.c
    code/seg.c
    code/shield.c
    code/splay.c
    code/ss.c
    code/table.c
    code/trace.c
    code/traceanc.c
    code/tract.c
    code/tree.c
    code/version.c
    code/vm.c
    code/walk.c
)
set(POOLS ${AMC} ${AMS} ${AWL} ${LO} ${MV2} ${MVFF} ${SNC})
set(MPM ${MPMCOMMON} ${MPMPF} ${POOLS} ${PLINTH})


# Varieties
#
# MPS varieties (HOT, COOL, etc.) probably map to CMake "Build
# Configurations"
# <https://cmake.org/cmake/help/latest/manual/cmake-buildsystem.7.html#build-configurations>.
#
# FIXME: I'm just mapping variety.cool (Debug) and variety.hot
# (Release) for now, but we need to add variety.rash at least for
# automatic comparison with hot.  It would be good to see if
# variety.cold builds and runs too.

add_compile_definitions(
  $<$<CONFIG:Debug>:CONFIG_VAR_COOL>
  $<$<CONFIG:Release>:CONFIG_VAR_HOT>
)

# Override CMake's default flags because we specify all flags.
message(VERBOSE "Setting CMAKE_C_FLAGS from ${CMAKE_C_FLAGS} to ${CFLAGSCOMPILER}")
list(JOIN CFLAGSCOMPILER " " CMAKE_C_FLAGS)
message(VERBOSE "Setting CMAKE_C_FLAGS_DEBUG from ${CMAKE_C_FLAGS_DEBUG} to ${CFLAGSDEBUG}")
list(JOIN CFLAGSDEBUG " " CMAKE_C_FLAGS_DEBUG)
message(VERBOSE "Setting CMAKE_C_FLAGS_RELEASE from ${CMAKE_C_FLAGS_RELEASE} to ${CFLAGSOPT}")
list(JOIN CFLAGSOPT " " CMAKE_C_FLAGS_RELEASE)


# Targets
#
# Just using gcbench as an example target for now.

#add_library(mps OBJECT code/mps.c)
#target_compile_options(mps PRIVATE ${CFLAGSCOMPILERSTRICT})

add_library(mps STATIC
  $<$<CONFIG:Release>:code/mps.c>
  $<$<CONFIG:Debug>:${MPM}>
)
target_compile_options(mps PRIVATE ${CFLAGSCOMPILERSTRICT})

add_library(mpsplan STATIC ${PLINTH})
target_compile_options(mpsplan PRIVATE ${CFLAGSCOMPILERSTRICT})

add_executable(gcbench code/gcbench.c ${FMTDYTST} ${TESTLIB} ${TESTTHR})
target_compile_options(gcbench PRIVATE ${CFLAGSCOMPILERSTRICT})
# FIXME: Should be C89 but CMake doesn't know it.
# FIXME: How to set this for all targets not just gcbench?
# target_compile_features(gcbench PRIVATE c_std_90)


# Tests
#
# Tests specified with ``add_test`` can be run with e.g.
# ``make -C cool test``.
#
# See <https://cmake.org/cmake/help/book/mastering-cmake/chapter/Testing%20With%20CMake%20and%20CTest.html>.

include(CTest)

add_executable(amcss code/amcss.c ${TESTLIB} ${FMTDYTST})
# FIXME: target_compile_options(amcss PRIVATE ${CFLAGSCOMPILERLAX}) etc. ?
target_link_libraries(amcss PRIVATE mps)
add_test(NAME amcss COMMAND amcss)

add_executable(abqtest code/abqtest.c ${TESTLIB})
target_link_libraries(abqtest PRIVATE mps)
add_test(NAME abqtest COMMAND abqtest)
