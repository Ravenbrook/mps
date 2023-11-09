=======================
Rules for writing style
=======================

:Tag: rule.style
:Author: Gareth Rees <mailto:gdr@ravenbrook.com>
:Organization: Ravenbrook Limited
:Date: 2001-12-02
:Readership: anyone
:Confidentiality: public
:Copyright: See `C. Copyright and License`_


1. Introduction
===============

This document is a set of rules which apply to manuals and other
technical documentation aimed at users of software. The rules are
intended for use with an inspection procedure [Gilb-1995]_ but can
also be used for general guidance and review.

These rules were originally stated in [Corman-2001-11-27]_.

The intended readership is Ravenbrook staff, but these rules can used by
anyone.

This document is not confidential.


2. Rules
========

_`.explicit`: Give explicit instructions on how to carry out a task.

_`.indicative`: Use the indicative mood.

_`.lists`: Use numbered lists when sequence is important, unnumbered
otherwise.

_`.may`: Be careful with "may".

_`.task`: Organize user documentation by task.

_`.present`: Use the present tense where possible.


3. Justification and commentary
===============================

_`.explicit.just`: Don't just state the goal of the task and leave the
reader to figure out how to do to it: some readers might not know what
to do, or might decide to do the wrong thing.

_`.indicative.just`: Avoid the subjunctive mood (clauses introduced
with "could", "should", or "would").  This leaves the reader uncertain
as to whether to do the task or not.  It's OK to use "should" as a
technical term conveying a level of requirement, if you've defined it
as such.

_`.lists.just`: [I don't know the justification for this -- GDR
2001-12-02]

_`.may.just`: The word "may" is ambiguous; it means both "be allowed
to" and "possibly". Use "might" for the second sense.  It's OK to use
"may" as a technical term conveying a level of requirement, if you've
defined it as such.

_`.task.just`: Organization by task makes it easy for the reader to
find the section corresponding to the task they want to perform, and
makes it more likely that they'll carry out the task correctly.

_`.tense.just`: [I don't know the justification for this -- GDR
2001-12-02]


A. References
=============

.. [Corman-2001-11-27]
    "General comments on DTI Advanced Admin Guide" (e-mail message);
    Tony Corman;
    Perforce Software;
    2001-11-27;
    <https://info.ravenbrook.com/mail/2001/11/27/21-05-45/0.txt>.

.. [Gilb-1995]
    “Software Inspection”;
    Tom Gilb, Dorothy Graham;
    Addison-Wesley_;
    1995;
    ISBN 0-201-63181-4.

.. _`Addison-Wesley`: http://www.awl.com/


B. Document History
===================

==========  =====  ==================================================
2001-12-02  GDR_   Created based on [Corman-2001-11-27]_.
2023-01-26  RB_    Integrated to MPS Git and prepared for public use.
==========  =====  ==================================================

.. _GDR: mailto:gdr@ravenbrook.com
.. _RB: mailto:rb@ravenbrook.com


C. Copyright and License
========================

Copyright © 2001-2023 `Ravenbrook Limited <https://www.ravenbrook.com/>`_.

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
