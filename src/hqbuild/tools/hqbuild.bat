@REM impl.bat.hqbuild
@REM $HopeName: !hqbuild:tools:hqbuild.bat(MMdevel_sw_eq.1) $
@REM Copyright (C) 1996 Harlequin Group, all rights reserved
@REM Called by SWIG autobuild system
@
@REM we expect whatcom to have set MSVCNT and possibly MSMASM
IF NOT %MSMASM%X == X SET PATH=%MSMASM%\bin;%PATH%
SET PATH=%MSVCNT%\bin;%PATH%
SET LIB=%MSVCNT%\lib
SET INCLUDE=%MSVCNT%\include
@REM Silently throw away all but first argument
nmake /f %1.nmk
