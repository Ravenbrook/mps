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
#   cmake -B hot -DCMAKE_BUILD_TYPE=RELEASE --log-level=VERBOSE . && cmake --build hot --verbose
#
# See GitHub issue #146 <https://github.com/Ravenbrook/mps/issues/146>
# for discussion.
#
# TODO: Incorporate %% instructions

# Tutorial says to put this in but does not give any advice about the
# argument, so I'm guessing based on feature documentation I've used.
cmake_minimum_required(VERSION 3.15)

project(mps LANGUAGES C)


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
set(TESTLIB testlib.c)
set(TESTTHR testthrix.c)  # FIXME: Platform specific, varies by OS
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


# Varieties
#
# MPS varieties (HOT, COOL, etc.) probably map to CMake "Build
# Configurations"
# <https://cmake.org/cmake/help/latest/manual/cmake-buildsystem.7.html#build-configurations>.
# I'm just mapping Debug and Release for now.

# Unset CMake's default flags
set(CMAKE_C_FLAGS_DEBUG "")
set(CMAKE_C_FLAGS_RELEASE "")
set(CMAKE_C_FLAGS "")

add_compile_definitions(
  $<$<CONFIG:Debug>:CONFIG_VAR_COOL>
  $<$<CONFIG:Release>:CONFIG_VAR_HOT>
)


# Compilers

if (CMAKE_C_COMPILER_ID STREQUAL "GNU")

  message(VERBOSE "compiler.gc detected")

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
  
elseif (CMAKE_C_COMPILER_ID STREQUAL "Clang")

  message(VERBOSE "compiler.cl detected")

  # Lifted from variables in ll.gmk
  set(CFLAGSDEBUG, -O0 -g3)
  set(CFLAGSOPT, O2 -g3)
  set(CFLAGSCOMPILER,
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
  set(CFLAGSCOMPILERSTRICT,
    -std=c89
    -pedantic
  )

elseif (CMAKE_C_COMPILER_ID STREQUAL "MSVC")

  message(VERBOSE "compiler.mv detected")

  # Lifted from variables in mv.nmk.
  set(CFLAGSDEBUG, /MTd /Od)
  set(CFLAGSOPT, /O2 /MT)
  set(CFLAGSCOMPILER,
    # from CFLAGSCOMMONPRE in commpre.nmk
    /nologo
    # FIXME: PFMDEFS?
    # FIXME: CFLAGSTARGETPRE?
    /W4 /WX
  )
  set(CFLAGSCOMPILERSTRICT, )

  # FIXME: LINKFLAGS* from mv.nmk

else ()

  message(WARNING "Unknown compiler toolchain.  Compiling without warning flags.")

endif ()

add_compile_options(
  "$<$<CONFIG:Debug>:${CFLAGSDEBUG}>"
  "$<$<CONFIG:Release>:${CFLAGSOPT}>"
  ${CFLAGSCOMPILER}
  # FIXME: CFLAGSCOMPILERSTRICT or LAX
)


# Targets
#
# Just using gcbench as an example target for now.

add_executable(gcbench gcbench.c ${FMTDYTST} ${TESTLIB} ${TESTTHR})
target_link_libraries(gcbench PRIVATE m)
# FIXME: Should be C89 but CMake doesn't know it.
# FIXME: How to set this for all targets not just gcbench?
# target_compile_features(gcbench PRIVATE c_std_90)

#add_library(mps OBJECT mps.c)
#target_compile_options(mps PRIVATE ${CFLAGSCOMPILERSTRICT})

add_library(mps STATIC
  $<$<CONFIG:Release>:mps.c>
  $<$<CONFIG:Debug>:${MPM}>
)
target_compile_options(mps PRIVATE ${CFLAGSCOMPILERSTRICT})
