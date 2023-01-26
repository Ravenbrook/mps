===========================
Rules for source code style
===========================

:Tag: rule.code.style
:Author: Gareth Rees
:Organization: Ravenbrook Limited
:Date: 2001-05-25
:Readership: programmers
:Confidentiality: public
:Copyright: See `C. Copyright and License`_


1. Introduction
===============

This document is a style guide for source code. It is intended for use
with an inspection procedure [Gilb-1995]_ but can also be used for
general guidance and review.

This style guide collects rules to do with code layout and style; we
have these rules so that documents are consistent with themselves
(`rule.generic.self`_) and each other
(`rule.generic.other`_). However, these details don't often cause
defects, so they are collected here to avoid other rule sets becoming
cluttered with unimportant details [RB-2001-05-24]_.

.. _rule.generic.self: rule.generic.rst#2-rules
.. _rule.generic.other: rule.generic.rst#2-rules

The intended readership is Ravenbrook staff, but these rules can used by
anyone.

This document is not confidential.


2. General style rules
======================

_`.layout`: When editing someone else's document, follow the layout
conventions of that document (`rule.generic.self`_).


3. Rules for C-like languages
=============================

These rules apply to language with C-like syntax (for example, C, C++,
Java, Perl, Python).

_`.binop`: Put a single space either side of a binary operator (except
`.comma`_).

_`.block`: When a block is delimited by braces, put the opening brace
at the end of the line introducing the block, closing brace on a line
by itself, at same intendation level as the line introducing the
block.

_`.colon`: No space before a colon, one space after.

_`.comma`: No space to the left of a comma, one space to the right.

_`.comment`: Align comments with the code they refer to.

_`.control`: One space between a control structure (``if``, ``while``,
``for``) and the opening parenthesis of its arguments.

_`.function`: No space between a function and the opening parenthesis
of its arguments.


A. References
=============

.. [GDR-2001-05-23a]
    "Rules for source code in C-like languages";
    Gareth Rees;
    `Ravenbrook Limited`_;
    2001-05-23.

.. [GDR-2001-05-23b]
    "Rules for source code in Python";
    Gareth Rees;
    `Ravenbrook Limited`_;
    2001-05-23.

.. [Gilb-1995]
    “Software Inspection”;
    Tom Gilb, Dorothy Graham;
    Addison-Wesley_;
    1995;
    ISBN 0-201-63181-4.

.. [RB-2001-05-24]
    "Re: Rules for Python" (e-mail message);
    Richard Brooksby;
    `Ravenbrook Limited`_;
    2001-05-24;
    <https://info.ravenbrook.com/mail/2001/05/24/12-34-31/0.txt>.

.. _`Addison-Wesley`: http://www.awl.com/


B. Document History
===================

==========  =====  ==================================================
2001-04-22  GDR_   Created based on rules originally in
                   [GDR-2001-05-23a]_ and [GDR 2001-05-23b]_,
		   following the advice in [RB-2001-05-24]_.
==========  =====  ==================================================

.. _GDR: mailto:gdr@ravenbrook.com


C. Copyright and License
========================

Copyright © 1998-2023 `Ravenbrook Limited <https://www.ravenbrook.com/>`_.

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
