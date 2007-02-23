CMU Common Lisp is a public domain implementation of Common Lisp.  Both
sources and executables are freely available via anonymous FTP; this software
is "as is", and has no warranty of any kind.  CMU and the authors assume no
responsibility for the consequences of any use of this software.  See
doc/release-notes.txt for a description of the state of the release you have.

The CMU Common Lisp project is no longer funded, so only minimal support is
being done at CMU.  There is a net community of CMU Common Lisp users and
maintainers who communicate via comp.lang.lisp and the cmucl-bugs@cs.cmu.edu
mailing list.

See "man cmucl" (man/man1/cmucl.1) for other general information.

Distribution:

CMU Common Lisp is only available via anonymous FTP.  We don't have the
manpower to make tapes.  These are our distribution machines:
    lisp-sun1.slisp.cs.cmu.edu (128.2.250.58)
    lisp-rt1.slisp.cs.cmu.edu (128.2.217.9)
    lisp-rt2.slisp.cs.cmu.edu (128.2.217.10)

Log in with the user "anonymous" and "username@host" as password (i.e. your
EMAIL address.)  When you log in, cd USING ONE SINGLE "cd" COMMAND to
/afs/cs/project/clisp/release.  If you have any trouble with FTP access, please
send mail to slisp@cs.cmu.edu.

The most recent version is: 17f
The old version is: 16f

The currently supported platforms are:
    alpha_osf1:
	DEC Alpha workstations running OSF1.

    hp700_ux90:
        HP 700 series machines (based on the HPPA architecture) running HP/UX
        9.x.

    sgi_52:
	MIPS-based SGI workstations running Irix 5.x.

    sun4c_411:
        Sun SPARC machines running the the pre-Solaris BSD SunOS system,
        version 4.x.

    sun4c_53:
	Sun SPARC machines running Solaris/SunOS 5.x.


The release area holds gzip'ed tar files with names of the form:
    <version>-<platform>.tar.gz
    <version>-extra-<platform>.tar.gz
    <version>-runtime-<platform>.tar.gz

 -- The first file holds binaries and documentation for the standard Common
    Lisp portion of CMU CL.  This includes our version of the PCL
    implementation of the CLOS object system.  
 -- The `-extra' file contains the Hemlock editor, the Motif toolkit,
    the graphical debugger and the CLX interface to X11.
 -- The `-runtime' file contins one file: lib/runtime.core, which is a
    smaller substitute for lib/lisp.core.  See the "runtime distribution"
    section.

The installed sizes of the configurations are approximately:
    Basic: 15 megabytes
    Basic+Extra: 24 megabytes
    Runtime: 5.3 megabytes

For installation directions, see the section "site initialization".

FTP gzip'ed tar archives in binary mode.  To extract, "cd" to the
directory that is to be the root of the tree, then type:
    gzcat file.tar.gz | tar xf - .

If poor network connections make it difficult to transfer a 5 meg file, the
release is also available split into 2 megabyte chunks, suffixed `.0', `.1',
etc.  To extract from multiple files, use:
    cat file.tar.gz.* | gunzip | tar xf - .

The release area also contains source distributions and other binary
distributions.  A listing of the current contents of the release area is in
FILES.  Major release announcements will be made to comp.lang.lisp.


Site initialization:

To run CMU CL, place bin/ in PATH and setenv CMUCLLIB to point to lib/.  The
file lib/site-init.lisp contains site-specific initialization, such as setting
of the site name.  Any site-specific initialization should be placed in this
file; this file can be compiled.  See bin/sample-wrapper for a shell script
template that sets up environment variables and then runs CMU CL.  CMUCLLIB now
defaults to /usr/local/lib/cmucl/lib.

You may want to have your EMACS maintainer place doc/cmu-user.info in the info
root, or you can setenv INFOPATH to include the doc/ directory.  
[NOTE: the Info version of the manual has become increasingly out of date
because we can't find a working version of LaTexInfo.]

To load in CLX, the Motif interface and windowing debugger, or Hemlock and save
an augmented Lisp image, run lib/config.  This runs `lisp' on
`lib/config.lisp', which uses an interactive dialog to determine what systems
to load and where to save the result.  The default output is to overwrite
library:lisp.core.  To avoid overwriting the running Lisp image, any existing
image is renamed to `lisp.core.BAK'; this file may be manually deleted to save
disk space.


MOTIF NOTE:

The Motif server doesn't work on Alpha/OSF1 yet.

On all platforms, the Motif interface should be considered somewhat
experimental.  The code has been compiled with type checking and debug info.
If Motif is loaded, the graphical inspector/debugger becomes the default, but
this can be disabled by:
  (setf interface:*interface-style* :tty)

In order to use Motif (and the graphical debugger) with X servers from non-OSF
vendors (like Sun) you may need to set the environment variable XKEYSYMDB to
point to the file lib/XKeysymDB.  The environment variable value should include
the file name XKeysymDB, not just the path.  Otherwise, you will get many error
messages every time a new connection is opened to the CMU CL motifd.  This file
is read by the X11R5 Xt in order to augment the keysym database with certain
OSF vendor keysyms that Motif wants to use.

If you get errors about being unable to start the Motif server, try:
 -- Setting DISPLAY to the full hostname, like:
        lisp-rt1.slisp.cs.cmu.edu:0
 -- Killing any motifd processes that didn't die when Lisp did.
 -- Deleting any .motif-socket-* files in /tmp.
 -- Make sure CMUCLLIB points to the lib/ directory, and that motifd is 
    in the lib/ directory.


Installation example (assuming the distribution files are in the cwd):

    % mkdir cmucl
    % cd cmucl
    % gzcat ../17c-sunos.tar.gz | tar xf -
    % gzcat ../17c-extra-sunos.tar.gz | tar xf -
    % cd lib
    % setenv CMUCLLIB `pwd`
    % setenv XKEYSYMDB `pwd`/XKeysymDB
    % cd ../bin
    % setenv PATH $PATH":"`pwd`

Now you can run the basic Lisp:
    % lisp
    CMU Common Lisp 17c, running on lisp-sun1.slisp.cs.cmu.edu
    Send bug reports and questions to your local CMU CL maintainer, or to
    cmucl-bugs@cs.cmu.edu.
    Loaded subsystems:
	Python 1.0, target SPARCstation/Sun 4
	CLOS based on PCL version:  September 16 92 PCL (f)
    common-lisp-user> (quit)

Use config to load optional subsystems:
    % ../lib/config
    ; Loading #p"library:config.lisp".
     1: specify result file (currently "library:lisp.core")
     2: toggle loading of the CLX X library, currently enabled.
     3: toggle loading of Motif and the graphical debugger, currently enabled.
     4: toggle loading the Hemlock editor, currently enabled.
     5: specify some site-specific file to load.
     6: configure according to current options.
     7: abort the configuration process.

    Option number: 6

    ;; Loading #p".../lib/subsystems/clx-library.sparcf".
    ;; Loading #p".../lib/subsystems/clm-library.sparcf".
    ;; Loading #p".../lib/subsystems/hemlock-library.sparcf".
    Saved ".../foo/lib/lisp.core" as ".../foo/lib/lisp.core.BAK".
    [Doing purification: Done.]
    [Undoing binding stack... done]
    [Saving current lisp image into .../foo/lib/lisp.core:
    Writing 14804416 bytes from the Read-Only space at 0x00200000.
    Writing 3542680 bytes from the Static space at 0x0C000000.
    Writing 1464 bytes from the Dynamic space at 0x10000000.
    done.]
    % 


Runtime distributions:

The <version>-runtime-<platform>.tar.gz distribution contains a small alternate
version of lib/lisp.core called lib/runtime.core.  If you say:
    lisp -core lib/runtime.core

you'll get a Lisp process into which you can load a compiled CMU CL program.

The runtime image does not contain PCL (CLOS), the compiler, or the full
Common Lisp eval.  EVAL only supports function calls and the (non-lambda)
FUNCTION, QUOTE, PROGN, EVAL-WHEN and SETQ special forms.  Macros are o.k. as
long as they expand into one of the supported forms.

Additional compactness is gained by byte-compiling some language features that
are primarily used for development and debugging, like the reader.

Probably the main use for the runtime image is to load in some large
application, saving the result with SAVE-LISP.  This core image can then
replace lisp.core in distributions based on CMU CL.  The SAVE-LISP
:INIT-FUNCTION argument can be used to replace the top-level Lisp prompt with
the function of your choice.

Note that the size of the loaded application can be substantially reduced by
using non-default compilation policies, like:
    (declaim (optimize (speed 3) (safety 0) (debug 0)))

See the "CMU Common Lisp User's Manual" for more discussion of compilation
policy, byte compilation, etc.

You can reduce the size of your distribution somewhat more by deleting the
doc/ directory containing the CMU CL documentation.  Really, the entire tree
and CMUCLLIB environment variable are unnecessary.  All you need is the "lisp"
executable and the core file, which can be started by using the -core option,
like in:
    lisp -core my.core


SunOS/SPARC Notes:

With this release, there are now two Sun versions, one for SunOS 4.x and one
for SunOS 5.x (Solaris.)

At least 16 meg of memory is recommended, and more is better.  Your system
maintainer may need to configure extra paging space for large Lisp
applications.

It is not possible to mmap a file in a tmpfs filesystem.  If /tmp is a "tmpfs"
filesystem, then you must setenv CMUCL_EMPTYFILE to the pathname of some file
(in a normal filesystem) that can be used instead of /tmp/empty.  The "df"
command will show tmpfs filesystems as mounted on "swap".  If this problem
exists on your system, lisp will get an error like:
    mapin: mmap: Invalid argument
    ensure_space: Failed to validate 67108864 bytes at 0x01000000


HP/UX (HPPA) Notes:

CMU CL only runs with HP/UX version 9.x.  Currently there are two major quirks:
 -- Huge amounts of swap space are required, since every time a lisp process
    forks, paging space is reserved for the entire image (~20 meg per fork.)
    Even when one Lisp has started successfully, any subsequent RUN-PROGRAM
    could possibly fail.  Probably you need at least 75 meg for reliable
    operation.  Run /etc/swapinfo.  If swapping hasn't already been enabled on
    the unix filesystem partitions, you can increase paging space using
    /etc/swapon or by modifying /etc/checklist.  (Note that a runtime-only
    image will require much less paging space.)
 -- Due to a HP/UX bug, the normal "lisp" startup program has been replaced by
    a wrapper which creates a copy on /tmp of the actual startup program (now
    kept in lib/lisp.orig)  If a lisp process terminates violently, spurious
    copies of /tmp/lisp.copy.a* may be left around.  Also, due to some weird
    terminal I/O interaction, if you interrupt Lisp when it is running in a
    shell script, Lisp keeps trying to read from the terminal even though the
    shell is too.  Note that the CMUCLLIB environment variable is used to
    locate lisp.orig (the actual startup code.)
 -- load-foreign does work now, but it only recognizes one particular object
    file format (which is not the one used by the version of gcc we have.)  
    "file foo.o" must print:
	foo.o:		PA-RISC1.1 relocatable object


Alpha/OSF1 and SGI/Irix notes:

IMPORTANT Alpha/OSF1 NOTE: CMU CL requires lazy allocation of paging space.
If you get "map: not enough space" errors on startup, then you (or your
sysadmin) need to do "rm /sbin/swapdefault" to enable lazy allocation.

LOAD-FOREIGN has not been implemented for these platforms yet.  Feel free to
add support to code/foreign.lisp (and let us know.)

Motifd seems to work on Irix, but has not been tested.  Motifd does not run on
Alpha/OSF1.  At a minimum, this would require fixing places that currently
use "long" to mean 32 bits.


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
source code files used to build the current version:
    <version>-source.tar.gz (3 meg)

A brief overview of the source tree:
    Totally machine-independent compiler code:
	.../compiler/*.lisp
    See especially node.lisp and ir1tran.lisp for the front end.  vop.lisp,
    vmdef.lisp and ir2tran.lisp for the back end.

    Stuff that is dependent on our choice of object format, but not
    particularly machine-dependent:
	.../compiler/generic/*.lisp

    Compiler back-end for the MIPS, SPARC, HPPA and Alpha
	.../compiler/mips/*.lisp
	.../compiler/sparc/*.lisp
	.../compiler/hppa/*.lisp
	.../compiler/alpha/*.lisp

    Miscellaneous Lisp run-time code:
	.../code/*.lisp

    C run-time code:
	.../lisp/*


There is also a distribution of documentation sources:
    documents.tar.gz

The contents of the ".../internals/" directory in the document distribution may
be of interest.  This is a very drafty 160 page version of an internal design
document.  Some of the "tex" files may be more humanly readable, since many
formatting commands need to be added.  There is some inaccurate (dated)
material in the compiler description.

See ".../internals/architecture.tex (the System Architecture chapter in the
internals document) for more information on the source tree organization.


Building from sources:

Recompiling CMU CL is somewhat arcane, and also requires a machine with a
working CMU CL and lots of memory and paging space.  The minimum amount of
memory and paging space will depend on the platform, but 32meg of memory and
100 meg of paging space is probably safe.  You will also need at least 100meg
of free disk space for source and object files.

The build procedure is somewhat involved.  In particular, some C header files
are actually generated by the Lisp compilation process.  See
internals/architecture.tex in the documents distribution for some discussion of
building.  Also, see tools/build-and-install, a script that David Axmark
developed for automating the process.

Porting CMU CL to a new architecture requires much, much more than just
recompilation, since it requires writing a new compiler backend.  Porting to a
new Unix is easy in comparison, but still not very easy, since Lisp source code
still needs to be modified.
