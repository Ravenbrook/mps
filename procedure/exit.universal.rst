==================================================
Memory Pool System exit criteria for all documents
==================================================

:tag: exit.universal
:type: rule
:status: incomplete
:author: Gavin Matthews
:organization: Harlequin Limited
:date: 1996-08-08
:confidentiality: public
:copyright: See `C. Copyright and License`_
:readership: MPS developers

_`.scope`: These exit criteria should be used for all reviews (see
`proc.review.exit.criteria`_).

.. _`proc.review.exit.criteria`: review.rst#58-review-exit

_`.edit`: The editor has taken written action on all issues, recorded
in the review record; these may include rejecting the issue.

_`.quest`: All questions to the author (q) should have been answered
in mail to mm, and possibly in documentation.  ["Mail to mm" is
specific to the Harlequin MM Group.  GitHub comment responses are now
sufficient.  RB 2023-01-24]
 
_`.imp`: All improvement suggestions (I) should have resulted in one
of the following:

- _`.imp.edit`: Edit of another document; not if it is already
  approved.

- _`.imp.mail`: Passed on to someone responsible for the other
  document, and accepted.

- _`.imp.issue`: Escalated to an InfoSys issue.  ["InfoSys issue" is
  specific to the Harlequin MM Group or Ravenbrook.  GitHub issues are
  now sufficient.  RB 2023-01-24]

- _`.imp.reject`: Rejected by the editor for a documented reason.

_`.record`: All fields have been filled in in the review record,
including metrics, and estimated defects remaining.

_`.defects`: The estimated defects remaining is less than the
acceptable level for the document type.  In the absence of a more
specific level, use 2 major defects (see guide.review.class.major
[Needs importing and referencing.  RB 2023-01-23]) per page.

_`.rates`: The checking and logging rates did not exceed the optimum
rates by more that 20% on average. [Being too slow is ok.]

_`.veto.author`: The author does not wish to veto exit.

_`.veto.leader`: The leader does not wish to veto exit.


A. References
-------------

.. [Gilb_93] "Software Inspection"; Tom Gilb, Dorothy Graham; Addison
             Wesley; 1993; ISBN 0-201-63181-4; book.gilb93.


B. Document History
-------------------

Full document history before to 2001 has been lost because this
document was maintained in "Spring", a Lotus Notes information system
at Harlequin Limited.  Ravenbrook Limited did not receive a full
revision history when it acquired the MPS from Harlequin's successor.

==========  =====  ==================================================
1996-08-08  GRM    Created.
2001-10-09  NDL    Imported to Perforce.
2023-01-24  RB_    Integrated to MPS Git and prepared for public use.
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
