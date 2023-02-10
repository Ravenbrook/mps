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
#   cmake -DCMAKE_BUILD_TYPE=Debug . && make VERBOSE=1 clean all
#
# See GitHub issue #146 <https://github.com/Ravenbrook/mps/issues/146>
# for discussion.

# Tutorial says to put this in but does not give any advice about the
# argument, so I'm guessing based on feature documentation I've used.
cmake_minimum_required(VERSION 3.15)

project(mps LANGUAGES C)


# Common definitions
#
# Lifted from comm.gmk section "Common definitions" to define a few of
# the "parts".

set(TESTLIB testlib.c)
set(FMTDYTST fmtdy.c fmtno.c fmtdytst.c)
set(TESTTHR testthrix.c) # FIXME: Platform specific, varies by OS


# Targets
#
# Just using gcbench as an example target for now.

add_executable(gcbench gcbench.c ${FMTDYTST} ${TESTLIB} ${TESTTHR})
target_link_libraries(gcbench PRIVATE m)
# FIXME: Should be C89 but CMake doesn't know it.
# FIXME: How to set this for all targets not just gcbench?
target_compile_features(gcbench PRIVATE c_std_90)


# Varieties
#
# MPS varieties (HOT, COOL, etc.) probably map to CMake "Build
# Configurations"
# <https://cmake.org/cmake/help/latest/manual/cmake-buildsystem.7.html#build-configurations>.
# I'm just mapping Debug and Release for now.

add_compile_definitions(
  $<$<CONFIG:Debug>:CONFIG_VAR_COOL>
  $<$<CONFIG:Release>:CONFIG_VAR_HOT>
)


# Compilers
#
# FIXME: How to set these for *all* targets, not just gcbench?

if (CMAKE_C_COMPILER_ID STREQUAL "GNU")

  message(VERBOSE "compiler.gc detected")

  # Lifted from CFLAGSCOMPILER in gc.gmk

  target_compile_options(gcbench PRIVATE
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
    -pedantic
  )

elseif (CMAKE_C_COMPILER_ID STREQUAL "Clang")

  message(VERBOSE "compiler.cl detected")

  # Lifted from CFLAGSCOMPILER in ll.gmk

  target_compile_options(gcbench PRIVATE
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
    -pedantic
  )

elseif (CMAKE_C_COMPILER_ID STREQUAL "MSVC")

  message(VERBOSE "compiler.mv detected")

  # Lifted from CFLAGSCOMMONPRE in mv.nmk.

  target_compile_options(gcbench PRIVATE /W4 /WX)

else ()

  message(WARNING "Unknown compiler toolchain.  Compiling without warning flags.")

endif ()


