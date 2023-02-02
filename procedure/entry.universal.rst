=================================================
Memory Pool System entry criteria for all reviews
=================================================

:tag: entry.universal
:type: rule
:status: incomplete
:author: Gavin Matthews
:organization: Harlequin Limited
:date: 1996-07-24
:confidentiality: public
:copyright: See `C. Copyright and License`_
:readership: MPS developers

_`.scope`: These entry criteria are to be used for all reviews (see
`proc.review.entry.criteria`_).

_`.author`: The author_ of the document agrees to the review.

_`.reason`: The change must include, or permanently link to as a
`source`_, the reason the change is needed, expressed in terms of
requirements.

- On GitHub, this can be the GitHub issue linked via the pull request.

_`.reviewable`: It must be feasible to review the change using the
review procedure (`proc.review`_).  See `proc.review.plan.tactics`_.

_`.source-available`: The `source documents`_ are available in
writing.  [What is the purpose of "in writing" here and how to express
it now?  RB 2023-01-24]

_`.source-approved`: The `source documents`_ will have `exited review`_.
Failing this, they should be mini-reviewed and the fact noted.
Failing this, they should be marked as "UNREVIEWED".

_`.rules-available`: The relevant rules are available in writing.

_`.rules-approved`: The rule documents have exited review.  Failing
this, see `.source-approved`_.

_`.brief-check`: The leader has performed a cursory examination and
has found no more than one major defect (see guide.review.class.major
[Needs importing and reference.  RB 2023-01-23]) per page.  [This
should go further up the list.  RB 2023-01-24]

_`.plan`: There is a procedure for the review, including checking
rates for this type of document.

_`.training`: The leader has been formally trained and certified in
Inspection.  [This is unrealistic in the public MPS.  What can we say?
RB 2023-01-24]

_`.auto-check`: All automatic checks have been performed on the
product document, and it has passed.  [What counts as "all"?  This
means all the ones that are implemented in the MPS, e.g.
``make test``, CI checks, checks in ``tool/*``, etc.  RB 2023-01-24]

.. _`proc.review`: review.rst
.. _`proc.review.entry.criteria`: review.rst#51-review-entry
.. _author: review.rst#3-review-roles
.. _source: review.rst#6-documents
.. _source documents: source_
.. _exited review: review.rst#58-review-exit


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
1996-07-24  GRM    Created.
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
