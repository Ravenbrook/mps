#! /usr/local/bin/bash
#
# $HopeName: MMsrc!gathepco.sh(trunk.1) $
# Copyright (C) 2000 Harlequin Limited.  All rights reserved.

mkdir release
mkdir release/mm
mkdir release/mm/src
mkdir release/mmunix
mkdir release/mmunix/lii4gc
mkdir release/mmunix/lii4gc/we
mkdir release/mmunix/lii4gc/hi
mkdir release/mmunix/lii4gc/ti
cp -p mps.h release/mm/src
cp -p mpsacl.h release/mm/src
cp -p mpsavm.h release/mm/src
cp -p mpscepdl.h release/mm/src
cp -p mpscepvm.h release/mm/src
cp -p mpscmv.h release/mm/src
cp -p mpscmvff.h release/mm/src
cp -p mpsio.h release/mm/src
cp -p mpslib.h release/mm/src
cp -p mpstd.h release/mm/src
cp -p mpswin.h release/mm/src
cp -p lii4gc/we/mmsw.a release/mmunix/lii4gc/we
cp -p lii4gc/hi/mmsw.a release/mmunix/lii4gc/hi
cp -p lii4gc/ti/mmsw.a release/mmunix/lii4gc/ti
