CMU Common Lisp is a public domain implementation of Common Lisp.  Both
sources and executables are freely available via anonymous FTP; this software
is "as is", and has no warranty of any kind.  CMU and the authors assume no
responsibility for the consequences of any use of this software.  See
doc/release-notes.txt for a description of the state of the release you have.

CMU Common Lisp is available for:
    HPPA "snake" workstations running HPUx 9.X
    Sun SPARC workstations running SunOS (but not Solaris)
    DECstations (pmaxes) running Mach.

There are also frozen versions for Mach on the SPARC and IBM RT.

To get CMU Common Lisp on facilities Mach machines, run:
    /usr/cs/etc/modmisc -release beta - cs.misc.cmucl

This establishes /usr/misc/.cmucl as a symbolic link to the release area.
In your .login, add CMU CL to your path:
    setpath -i /usr/misc/.cmucl

Then run "lisp".  Note that the first time you run Lisp, it will take AFS
several minutes to copy the image into its local cache.  Subsequent starts
will be much faster.

Instead of using modmisc, you can create the symbolic links by hand, or run out
of the AFS release area.  This method of installation is necessary on HPUx and
SunOS machines.  Put this in your .login shell script:
    setenv CMUCLLIB "/afs/cs/misc/cmucl/@sys/beta/lib"
    setpath -i /afs/cs/misc/cmucl/@sys/beta

After setting your path, "man cmucl" will give an introduction to CMU CL and 
"man lisp" will describe command line options. 

See /usr/misc/.cmucl/doc for release notes and documentation.  Rather old
hardcopy documentation is available as tech reports in the document room.

Send bug reports and questions to cmucl-bugs@cs.cmu.edu.  If you send a bug
report to gripe, they will just forward it to this mailing list.

See "man cmucl" (man/man1/cmucl.1) for other general information.

SunOS credit:

The SunOS support was written by Miles Bader and Ted Dunning.

Site initialization:

To run CMU CL, place bin/ in PATH and setenv CMUCLLIB to point to lib/.  The
file lib/site-init.lisp contains site-specific initialization, such as setting
of the site name.  Any site-specific initialization should be placed in this
file; this file can be compiled.  See bin/sample-wrapper for a shell script
template that sets up environment variables and then runs CMU CL.  You may
want to have your EMACS maintainer place doc/cmu-user.info in the info root, or
you can setenv INFOPATH to include the doc/ directory.

Note: In order to use Motif (and the graphical debugger) with X servers from
non-OSF vendors (like Sun) you may need to set the environment variable
XKEYSYMDB to point to the file lib/XKeySymDB.  Otherwise, you will get many
error messages every time a new connection is opened to the CMU CL motifd.
This file is read by the X11R5 Xt in order to augment the keysym database with
certain OSF vendor keysyms that Motif wants to use.  If you get errors about
being unable to start the Motif server, try setting DISPLAY to the full
hostname, like:
    lisp-rt1.slisp.cs.cmu.edu:0

and delete any .motif-socket-* files in /tmp.

SunOS/SPARC Notes:

With this release, CMU CL should require no special effort to run on Sparc10's
under SunOS.  Solaris is still  not supported.

At least 16 meg of memory is recommended, and more is better.  Your system
maintainer may need to configure extra paging space for large Lisp application.

It is not possible to mmap a file in a tmpfs filesystem.  If /tmp is a "tmpfs"
filesystem, then you must setenv CMUCL_EMPTYFILE to the pathname of some file
(in a normal filesystem) that can be used instead of /tmp/empty.  The "df"
command will show tmpfs filesystems as mounted on "swap".  If this problem
exists on your system, lisp will get an error like:
    mapin: mmap: Invalid argument
    ensure_space: Failed to validate 67108864 bytes at 0x01000000


Running CMU CL:

Run "lisp".  If Hemlock is loaded, you can also "lisp -edit".  Hemlock will
use X if the DISPLAY environment variable is defined, otherwise it will use
terminal i/o based on TERM and /etc/termcap.  If the Motif debugger is loaded,
it will be invoked on errors and by INSPECT.

Source availability:

Lisp and documentation sources are available via anonymous FTP ftp to any CMU
CS machine.  [See the "Distribution" section for FTP instructions.]  All CMU
written code is public domain, but CMU CL also makes use of two imported
packages: PCL and CLX.  Although these packages are copyrighted, they may be
freely distributed without any licensing agreement or fee.

The release area contains a source distribution, which is an image of all the
".lisp" source files used to build version 17c:
    17c-source.tar.Z (4.9 meg)

All of our files (including the release area) are actually in the AFS file
system.  On the release machines, the FTP server's home is the release
directory: /afs/cs.cmu.edu/project/clisp/release.  The actual working source
areas are in other subdirectories of "clisp", and you can directly "cd" to
those directories if you know the name.  Due to the way anonymous FTP
access control is done, it is important to "cd" to the source directory with a
single command, and then do a "get" operation.

Totally machine-independent compiler code:
    /afs/cs/project/clisp/src/beta/compiler/*.lisp
See especially node.lisp and ir1tran.lisp for the front end.  vop.lisp,
vmdef.lisp and ir2tran.lisp for the back end.

Stuff that is dependent on our choice of object format, but not
particularly machine-dependent:
    /afs/cs/project/clisp/src/beta/compiler/generic/*.lisp

Compiler back-end for the PMAX and SPARC:
    /afs/cs/project/clisp/src/beta/compiler/mips/*.lisp
    /afs/cs/project/clisp/src/beta/compiler/sparc/*.lisp

Miscellaneous Lisp run-time code:
    /afs/cs/project/clisp/src/beta/code/*.lisp

C run-time code:
    /afs/cs/project/clisp/src/beta/lisp/*

A very drafty version of an internal design document: (160 pages) Some of
the "tex" files may be more humanly readable, since many formatting
commands need to be added.  There is some inaccurate (dated) material in
the compiler description.
    /afs/cs/project/clisp/hackers/ram/docs/internals/*.tex
    /afs/cs/project/clisp/hackers/ram/docs/internals/design.ps

See hackers/ram/docs/internals/architecture.tex (the System Architecture
chapter in the internals document) for more information on the source tree
organization.
