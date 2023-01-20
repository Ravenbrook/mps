===================================
Memory Pool System review procedure
===================================

:tag: proc.review
:author: Richard Brooksby
:organization: Ravenbrook Limited
:date: 2023-01-19
:confidentiality: public
:copyright: See `C. Copyright and License`_
:readership: MPS developers
:status: draft


1. Introduction
---------------

This document will contain a procedure for reviewing changes to the
MPS.

This is a placeholder while we arrange to bring in documents from
outside the MPS Git repository and tree.  For background, see `GitHub
issue #95 <https://github.com/Ravenbrook/mps/issues/95>`_.

Since this document was created as part of `a project to migrate from
Perforce to Git (and GitHub)
<https://github.com/orgs/Ravenbrook/projects/1>`_, this procedure will
give specifics on conducting review via GitHub.  But the general
procedure is in no way specific to GitHub, Git, Perforce, or any other
tool.


[Notes for inclusion:
  - Review is not just (or even mostly) looking at diffs, though
GitHub encourages this idea.
  - History of MPS review.
  - Why review is so important to the MPS.
  - Review is not just for code.
  - Check against book.gilb93.handbook [Gilb_93]_.
  - Check against kpa.pr, [CMU/SEI-93-TR-025]_, p L3-97
]


2. Purpose
----------

The purpose of the review procedure is:

1. find and correct major defects

2. prevent future defects

A defect is a failure to meet requirements.

As with any procedure, you can vary this one to meet this purpose, but
you should probably read section [Insert reference to Rationale here.
RB 2023-01-20].


3. Rôles
--------

[Reconstructed from memory and common sense, but needs checking
against book.gilb93.  RB 2023-01-20]

Reviews must involve more than one person.  There must be at least one
checker who is not the author.

Each person can takes on a number of rôles.

Every role must be assigned to someone.

[See also book.gilb93.proc.*.  RB 2023-01-20]

_`.role.author`: wrote the work under review

_`.role.checker`: checks the work during review 

_`.role.editor`: edits the work to correct defects found by review

_`.role.leader`: organises the review

_`.role.quality`: edits process to prevent defects found by review

_`.role.scribe`: ensures defects are recorded

_`.role.chief`: [Chief Inspection Leader in book.gilb93.  Need to look
this up.  RB 2023-01-20]

_`.role.manager`: ensures adequate resources are assigned to review
[Project Manager in book.gilb93.  Need to look this up. RB 2023-01-20]

Each role has tasks during each phase.


4. Phases
---------

[Adapted from proc.review of 1995-08-02.  RB 2023-01-20]

_`.proc`: The following procedures are performed in order (expect
where specified).  Note that the people mentioned are unlikely to be
distinct.

#. _`.request`: Review commences when the Author requests that his
   document be reviewed.  There is no procedure for this.  [Relate to
   GitHub non-draft pull requests.  RB 2023-01-20]

#. _`.entry`: The Author and the Leader perform proc.review.entry.

#. _`.kickoff`: The Leader, the Checkers, and sometimes the Author
   perform proc.review.ko.

#. _`.checking`: The Checkers perform proc.review.check.

#. _`.logging`: The Leader, the Scribe, and the Checkers perform
   proc.review.log.

#. _`.brainstorm`: The Leader, the Scribe, the Checkers and usually
   the Author perform proc.review.brainstorm.  The followup is done by
   the Leader, and make take place any time after the brainstorm
   meeting.

#. _`.edit`: The Editor performs proc.review.edit.

#. _`.exit`: The Editor and the Leader perform proc.review.exit.


A. References
-------------

.. [CMU/SEU-93-TR-025] "Key Practices of the Capability Maturity
                       ModelSM, Version 1.1"; Mark C. Paulk,
                       Charles V. Weber, Suzanne M. Garcia, Mary Beth
                       Chrissis, Marilyn Bush; Software Engineering
                       Institute, Carnegie Mellon University; 1993-02;
                       <https://resources.sei.cmu.edu/asset_files/TechnicalReport/1993_005_001_16214.pdf>.

.. [Gilb_93] "Software Inspection"; Tom Gilb, Dorothy Graham; Addison
             Wesley; 1993; ISBN 0-201-63181-4; book.gilb93.


B. Document History
-------------------

==========  =====  ==================================================
2023-01-19  RB_    Created.
2023-01-20  RB_    Importing material from MM Group proc.review.
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
