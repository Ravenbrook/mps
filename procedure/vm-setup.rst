.. mode: -*- rst -*-

===================================================
Memory Pool System virtual machine setup procedures
===================================================

:author: Richard Brooksby
:organization: Ravenbrook Limited
:date: 2023-01-13
:confidentiality: public
:copyright: See `C. Copyright and License`_
:readership: MPS developers
:status: draft


1. Introduction
---------------

This document contains procedures for setting up virtual machines
suitable for development, building, and testing the Memory Pool
System.

The purpose is to allow an MPS developer to develop the MPS on one
physical machine while ensuring their work is correct on the MPS
target platforms.


2. Overview
-----------

There are two parts to a VM setup: the host setup and the guest
setup.

To keep things simple, these should be independent as far as possible,
partly for convenience, but partly to get reproducible results.  For
example, the way we set up a Windows guest on a Linux host should be
the same as the way we set up a Windows guest on a macOS host.

[Is it a good idea to duplicate CI environments?  Or is it a good idea
to *avoid* that to get more coverage?  RB 2023-01-13]

[Approach: We set up command-line non-graphical guests and connect to
them via their console or SSH or similar.  RB 2023-01-13]

[For development, we could share filesystems, or run Git on the guest,
or use something like Emacs TRAMP to work on the guest.  RB
2023-01-13]

[Discuss architecture emulation, e.g. qemu-system-aarch64 on x86_64
hosts etc. for smoke testing.  RB 2023-01-14]


3. Host setup
-------------

3.1. Linux host
...............

[virt-manager / virt-builder, LXD, VirtualBox (x86, x86_64 only)]

[LXD is appropriate for checking compilation against compiler
toolchains and OS variants, but you're still running on the same
kernel, so it's not really good for testing the MPS.  Neither is a
QEMU/KVM really, but it's much better emulation.  RB 2023-01-14]

[From an `earlier experiment <keybase://chat/ravenbrook#mps/2352>`_::

  lxc launch ubuntu:18.04 mps-build-ubuntu-18-amd64
  lxc exec mps-build-ubuntu-18-amd64 -- bash --login
  apt-get update
  apt-get install -y build-essential
  login -f ubuntu
  git clone --depth 1 https://github.com/Ravenbrook/mps.git
  cd mps
  ./configure --prefix=$PWD/prefix && make install && make test
  .......
  Tests: 27. All tests pass.

]


3.2. Windows host
..................

[VirtualBox (x86, x86_64 only)]


3.3. macOS host
...............

[VirtualBox (x86, x86_64 only)]


4. Guest setup
--------------

[Ref target platforms.]

[Where to get base images]

4.1. Linux guest
................

[Ubuntu or Debian, build-essential, git, clang/gcc]


4.2. Windows guest
..................

[Chocolatey, git, Visual Studio]

[I know from recent experience that installing Chocolatey first allows
a simple non-interactive installation of Visual Studio.  We can copy
Travis CI's setup.  RB 2023-01-14]

[It would be nice to have a remote login so that the build can be
driven remotely without having to looking at and click on Windows.  RB
2023-01-14]


4.3. macOS guest
................

[git, homebrew, Xcode]

[Running a macOS guest on anything other than Apple hardware is (a)
against the macOS T&Cs and (b) fiddly and unreliable.  That doesn't
make this section useless, since you might want to isolate your MPS
build and test environment, or test with multiple versions of macOS or
its toolchain, using macOS VMs on macOS.  RB 2023-01-14]

[Can you run x86_64 macOS VMs on Apple Silicon in any useful way?  RB
2023-01-14]


4.4. FreeBSD guest
..................

[git]


A. References
-------------

[No references yet.  RB 2023-01-13]


B. Document History
-------------------

==========  =====  ==================================================
2023-01-13  RB_    Created.
==========  =====  ==================================================

.. _RB: mailto:rb@ravenbrook.com


C. Copyright and License
------------------------

Copyright © 2023 `Ravenbrook Limited <https://www.ravenbrook.com/>`_.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

.. end
