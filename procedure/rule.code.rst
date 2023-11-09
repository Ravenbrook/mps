=====================
Rules for source code
=====================

:Tag: rule.code
:Author: Richard Brooksby <rb@ravenbrook.com>
:Organization: Ravenbrook Limited
:Date: 1998-06-30
:Readership: any programmer
:Confidentiality: public
:Copyright: See `C. Copyright and License`_


1. Introduction
===============

This document is a ruleset which can be applied to any source code. It
is intended for use with an inspection procedure [Gilb-1995]_ but can
also be used for general guidance and review. 


2. Rules
========

_`.adaptable`: Non-adaptable code may not be used where there is a more
adaptable alternative.

_`.assume`: Code may not make assumptions about the inner workings of other
modules.

_`.branch-edits`: Edits on a version or development branch must be easy to
merge.

_`.break`: There must be documented justification wherever rules are broken
to meet requirements.

_`.constraint`: Code constraints must be easy to modify.

_`.deps`: Dependencies must be marked and cross-referenced at both ends.

_`.design`: The code must not be the sole documentation of the design.

_`.independent`: Platform dependent code may not be used where there is a
platform independent alternative.

_`.justified`: The justification for the code must be clear or documented.

_`.limits`: All limitations of a body of code (such as a module) must be
exposed to clients of that code (in its interface).

_`.minimal`: The code must limit its functionality to what is required. 
Extra functionality which arises from generality must be disabled.

_`.req`: The code must meet its requirements.

_`.simple`: The code must be as simple as possible (to meet requirements).

_`.style`: Code must be written according to the relevant style guide.

_`.tricky`: The code must not use clever tricks.  Write straightforward code.

_`.width`: Code should be no wider than 72 characters.


3. Justification and commentary
===============================

_`.justification-adaptable`: [Nothing written yet.  Note that
“adaptability” is a measure of the cost of meeting new and changing
requirements. RB 1998-07-01]

_`.justification-assume`: [Nothing written yet.  RB 1998-07-01]

_`.justification-branch-edits`:  Edits on version branches will be
merged back to the master sources. Edits on development branches will be
merged back to the version branch or the master sources that they came
from.  It must be possible to do the merges reliably and without
introducing errors.  The goal is to make it possible to consider each
change separately, to decide which to merge, and for different people to
maintain the code along the version branch successfully.  The fix on the
version branch may be a workaround and a better fix may need to be
developed separately. 

To do so, follow the following rules: 

1. Don't delete stuff.  It will be hard to restore it.  Comment it out
   or skip it. 

2. Don't fiddle with the formatting of code or comments.  It creates
   bogus conflicts that create extra work when merging. 

3. Add comments explaining why a change was made (especially in version
   branches).  Sign the comments with your name and the date. Explain why
   you had to make the change.  Refer to defect reports when fixing them. 

Check in each defect fix separately.

Write change comments that summarize what you did, and the quality of
the change (is it a permanent fix, a temporary workaround or what).  Are
there any consequences that other people should know about? There's no
need to say which job you fixed: the fix will make that clear. 

The rule was introduced to fix job000070_.

.. _job000070: https://info.ravenbrook.com/project/p4dti/issue/job000070

_`.justification-break`: It's sometimes necessary to break the rules to
meet the requirements. For example, there might be an urgent change, or
you might need to introduce a dependency that could be avoided.  This is
OK, provided that the transgression is justified, which is to say, there
must be a convincing explanation of why you needed to break the rule.

_`.justification-constraint`: A constraint is something which limits the
generality of the code in some way.  The most common form of constraint
is a constant which bounds the size of something, for example, the
length of a fixed size array. This rule means that you must make it easy
to modify the size of the array, perhaps by defining the length as a
constant and using that everywhere.  (See also rule.generic.once_,
which backs this up.)

.. _rule.generic.once: /rule/generic#.once

_`.justification-deps`: If there is a dependency between two pieces of
code, especially if they are a long way apart, the dependency must be
marked at both ends, in such a way as the other end of the dependency
can be found.  It must be marked at the dependee end because a change
there will break the dependent code.  It must be marked at the dependent
end so that the dependency can be understood when modifying that code.

Changes in the presence of dependencies cause defects.  They are one of
the things that only the code “wizards” understand, so that only they
can modify the code successfully.

_`.justification-design`: [Nothing written yet.  RB 1998-07-01]

_`.justification-independent`: This rule is saying that you must write
platform independent code wherever you possibly can.  The most common
argument for platform dependent code is that it has better performance. 
Maybe. Platform-optimized code is only allowable if it has a documented
justification in terms of requirements (see the break rule).

_`.justification-justified`: This rule is saying that it must be clear
why the code is like it is.  So, if an obscure or counter-intuitive
structure is used, it must be explained.

This rule makes the code maintainable, which is to say, adaptable at low
cost and without introducing defects.  This rule ensures that the code
can be understood.

_`.justification-limits`: [Nothing written yet.  RB 1998-07-01]

_`.justification-minimal`: [Nothing written yet.  RB 1998-07-01]

_`.justification-req`:  This is a restatement of the generic/achieve
rule. The difference here is that the purpose of code is more often
stated directly in terms of customer requirements, and the code should
refer to and be consistent with those.

_`.justification-simple`: Write the simplest code you can.  Don't
confuse simplicity with elegance.  Don't try to be clever, try to be
defect free.  You will find this hard enough without making things
complicated.

_`.justification-style`: Almost any consistent style is better than a
mixture of styles, even if those styles are in some sense better.  A
common style should be used to make the code easy for people to read.
Code which is easy to read is easier to check and more likely to be
correct.

Ravenbrook's style guide is [GDR-2001-05-25]_.

_`.jusitification-tricky`: This is a variation of the simple rule,
designed to catch those cases where a tricky piece of code is argued by
the author to be “simpler” than a longer but more straightforward piece
of code.  For example::

    if (a = call(b)) {
        error("call failed");
    }

It might be argued that this is simpler than::

    a = call(b);
    if (a != RESULT_OK) {
        error("call failed");
    }

But the former is a "trick" of the C language and is therefore not
allowed.

Tricks of this sort are sources of defects because they are easily
misunderstood, and therefore will be modified incorrectly.

_`.jusitification-width`:  Many terminals, editors and mail user agents
have a standard width of 80 characters.  If code is too wide, then it
will be hard to read and edit using these tools and that will mean that
defects are introduced. In particular, we want to support: 

- Editing in vi with line numbers turned on.

- Editing in BBEdit without having to resize the window each time you
  open a file.

- Quoting source code in e-mail (possibly to two or three levels of
  quoting) without causing it to wrap and become unreadable. 

See [RB-2001-05-15]_.


A. References
=============

.. [GDR-2001-05-25]
    "Rules for source code style";
    `Gareth Rees`_;
    `Ravenbrook Limited`_;
    2001-05-25;
    <https://info.ravenbrook.com/rule/code/style>.

.. [Gilb-1995]
    "Software Inspection";
    Tom Gilb, Dorothy Graham;
    Addison-Wesley_;
    1995;
    ISBN 0-201-63181-4.
  
.. [RB-1998-06-30a]
    "General Code Ruleset";
    `Richard Brooksby`_;
    `Ravenbrook Limited`_;
    1998-06-30;
    <https://info.ravenbrook.com/doc/2000/05/09/rule-sets/code.html>.
  
.. [RB-1998-06-30b]
    "Rules for all documents";
    `Richard Brooksby`_;
    `Ravenbrook Limited`_;
    1998-06-30;
    <https://info.ravenbrook.com/rule/generic>.

.. [RB-2001-05-15]
    "Re: Code width" (e-mail message);
    `Richard Brooksby`_;
    `Ravenbrook Limited`_;
    2001-05-15;
    <https://info.ravenbrook.com/mail/2001/05/15/16-34-15/0/>.

.. _`Addison-Wesley`: http://www.awl.com/
.. _`Richard Brooksby`: mailto:rb@ravenbrook.com
.. _`Gareth Rees`: mailto:gdr@ravenbrook.com
  

B. Document History
===================

==========  =====  ==================================================
2001-04-22  GDR_   Created based on [RB-1998-06-30a]_.
2001-05-15  GDR_   Added width rule, based on [RB-2001-05-15]_.
2001-05-19  GDR_   Added branch-edits rule, based on RB's analysis in
                   job000070_.
2001-05-25  GDR_   Added reference to style guide [GDR-2001-05-25]_
                   to style rule.
2015-12-16  RB_    Converted to ReStructuredText and released under
                   Creative Commons license.
2023-01-26  RB_    Integrated to MPS Git and prepared for public use.
==========  =====  ==================================================

.. _GDR: mailto:gdr@ravenbrook.com
.. _RB: mailto:rb@ravenbrook.com


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
