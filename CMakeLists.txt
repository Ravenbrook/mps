# code/CMakeLists.txt -- CMake configuration for the MPS
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

# Tutorial says to put this in but does not give any advice about the
# argument, so I'm guessing based on feature documentation I've used.
#
# FIXME: The default CMake with Visual Studio 2017 is 3.12.  Check
# features to see if we can work with that.
cmake_minimum_required(VERSION 3.25)

project(mps LANGUAGES C)

# Some people say this defaults to "Release", but it's sometimes
# blank.
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

file(READ code/mpstd.h FILE_MPSTD_H)
string(
  REGEX REPLACE "\n[ \t]*#define[ \t]+MPS_PF_STRING[ \t]+" "\n#error "
  FILE_MPSTDE_C "${FILE_MPSTD_H}"
)
string(APPEND FILE_MPSTDE_C "int main(void) { return 0; }\n")
try_compile(
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


# FIXME: What about ANSI platform?
# FIXME: What about Freestanding platform?

if (OS STREQUAL "xc")

  # Lifted from xci3ll.gmk and xci6ll.gmk
  #
  # FIXME: Untested
  #
  # FIXME: xci3ll.gmk includes this line::
  #
  #   CC = clang -arch i386

  set(MPMPF
    lockix.c
    prmc${ARCH}.c
    prmc${OS}.c
    prmcxc${ARCH}.c
    protix.c
    prot${OS}.c
    span.c
    thxc.c
    vmix.c
  )
  
  set(TESTLIB testlib.c)
  set(TESTTHR testthrix.c)

  # FIXME: Is this necessary on macOS?
  link_libraries(m)

elseif (OS STREQUAL "li")

  # Lifted from lii3ll.gmk and lii6ll.gmk
  set(MPMPF
    lockix.c
    prmc${ARCH}.c
    prmcix.c
    prmc${OS}${ARCH}.c
    protix.c
    protsgix.c
    pthrdext.c
    span.c
    thix.c
    vmix.c
  )

  set(TESTLIB testlib.c)
  set(TESTTHR testthrix.c)

  link_libraries(m)

elseif (OS STREQUAL "fr")

  # FIXME: Fill this in from fr*.gmk

  set(TESTLIB testlib.c)
  set(TESTTHR testthrix.c)

  link_libraries(m)

elseif (OS STREQUAL "w3")

  # Lifted from w3i6mv.nmk
  #
  # FIXME: Untested
  
  set(MPMPF
    lock${OS}.c
    mpsi${OS}.c
    prmc${ARCH}.c
    prmc${OS}.c
    prmc${OS}${ARCH}.c
    prot${OS}.c
    sp${OS}${ARCH}.c
    th${OS}.c
    vm${OS}.c
  )

  set(TESTLIB testlib.c getoptl.c)
  set(TESTTHR testthr${OS}.c)

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

set(AMC poolamc.c)
set(AMS poolams.c)
set(AWL poolawl.c)
set(LO poollo.c)
set(SNC poolsnc.c)
set(POOLN pooln.c)
set(MV2 poolmv2.c)
set(MVFF poolmvff.c)
set(FMTDY fmtdy.c fmtno.c)
set(FMTDYTST fmtdy.c fmtno.c fmtdytst.c)
set(FMTHETST fmthe.c fmtdy.c fmtno.c fmtdytst.c)
set(FMTSCM fmtscheme.c)
set(PLINTH mpsliban.c mpsioan.c)
set(MPMCOMMON
    abq.c
    arena.c
    arenacl.c
    arenavm.c
    arg.c
    boot.c
    bt.c
    buffer.c
    cbs.c
    dbgpool.c
    dbgpooli.c
    event.c
    failover.c
    format.c
    freelist.c
    global.c
    land.c
    ld.c
    locus.c
    message.c
    meter.c
    mpm.c
    mpsi.c
    nailboard.c
    policy.c
    pool.c
    poolabs.c
    poolmfs.c
    poolmrg.c
    protocol.c
    range.c
    rangetree.c
    ref.c
    ring.c
    root.c
    sa.c
    sac.c
    scan.c
    seg.c
    shield.c
    splay.c
    ss.c
    table.c
    trace.c
    traceanc.c
    tract.c
    tree.c
    version.c
    vm.c
    walk.c
)
set(POOLS ${AMC} ${AMS} ${AWL} ${LO} ${MV2} ${MVFF} ${SNC})
set(MPM ${MPMCOMMON} ${MPMPF} ${POOLS} ${PLINTH})


# FIXME: There's probably a better way to do this
list(TRANSFORM MPM PREPEND "code/")
list(TRANSFORM PLINTH PREPEND "code/")
list(TRANSFORM TESTLIB PREPEND "code/")
list(TRANSFORM FMTDYTST PREPEND "code/")
list(TRANSFORM TESTTHR PREPEND "code/")


# Varieties
#
# MPS varieties (HOT, COOL, etc.) probably map to CMake "Build
# Configurations"
# <https://cmake.org/cmake/help/latest/manual/cmake-buildsystem.7.html#build-configurations>.
# I'm just mapping Debug and Release for now.

# Unset CMake's default flags
# message(VERBOSE "CMAKE_C_FLAGS = ${CMAKE_C_FLAGS}")
# message(VERBOSE "CMAKE_C_FLAGS_DEBUG = ${CMAKE_C_FLAGS_DEBUG}")
# message(VERBOSE "CMAKE_C_FLAGS_RELEASE = ${CMAKE_C_FLAGS_RELEASE}")
set(CMAKE_C_FLAGS_DEBUG "")
set(CMAKE_C_FLAGS_RELEASE "")
set(CMAKE_C_FLAGS "")

add_compile_definitions(
  $<$<CONFIG:Debug>:CONFIG_VAR_COOL>
  $<$<CONFIG:Release>:CONFIG_VAR_HOT>
)


add_compile_options(
  "$<$<CONFIG:Debug>:${CFLAGSDEBUG}>"
  "$<$<CONFIG:Release>:${CFLAGSOPT}>"
  ${CFLAGSCOMPILER}
  # FIXME: CFLAGSCOMPILERSTRICT or LAX
)


# Targets
#
# Just using gcbench as an example target for now.

#add_library(mps OBJECT mps.c)
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
target_link_libraries(amcss PRIVATE mps)
add_test(NAME amcss COMMAND amcss)

add_executable(abqtest code/abqtest.c ${TESTLIB})
target_link_libraries(abqtest PRIVATE mps)
add_test(NAME abqtest COMMAND abqtest)
