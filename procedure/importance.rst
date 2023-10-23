=============================
Importance in the MPS Project
=============================

:tag: importance
:author: Richard Brooksby
:organization: Ravenbrook Limited
:date: 2023-03-13
:confidentiality: public
:copyright: See `C. Copyright and License`_
:readership: developers, managers

1. Introduction
===============

This document defines *importance keywords* as used in the Memory Pool
System project.


2. Overview
============

_`.overview`: The MPS project uses the keywords `.critical`_,
`.essential`_, `.optional`_, and `.nice`_ to objectively define the
importance of a `.thing`_ to `.success`_.

_`.purpose`: Understanding and defining importance is essential to
prioritizing, planning, resource allocation, and especially *version
planning* ([RB_1999-05-20]_ §5).

_`.thing`: "Things" might be requirements, issues, defects, features,
deliveries, or anything else that can be identified.

_`.success`: "Success" means achieving the goals or meeting the
purpose of something, such as a function or design, and ultimately the
entire project.

_`.scope`: A `.thing`_ might be important locally or globally.  Where
the scope is unclear, we use a *to clause*.  For example, a mistake in
a procedure might cause the entire procedure to fail to achieve its
purpose, in which case we say it is "critical *to the procedure*" or
it might cause the entire project to fail, in which case we say it is
"critical *to the project*".

_`.urgency.not`: Importance is not the same as *urgency*.  Urgency is
a way of expressing how *soon* something must be done before it loses
its importance, and should be expressed using the language of time.
For example, it might be urgent to deliver a release before a certain
date, because if you don't meet the deadline it becomes *unimportant*
to deliver it: it's too late.

_`.urgency.scope`: However, it is possible for something to be
important to doing something time-bound.  For example, a feature could
be important *to a release* (`.scope`_).  If the release is urgent,
that implementation will also be urgent.  The importance should not
change when the release deadline changes.  The importance might change
when the *purpose* of the release changes.


3. Definitions
==============

_`.def`: Here are the definitions of the importance keywords.

_`.def.method`: Importance is defined by consequences.  The idea is
that you can determine the importance of something by considering what
will happen if it is not dealt with.

_`.def.leppard`: It's important to be clear about the units used to
specify the height of a henge.

[TODO: This might be clearer if we broke each one down into
e.g. ".critical.defect", ".essential.requirement" etc. Even better if
each sentence could be considered like a checklist. RB 2023-03-13]

_`.critical`: The entire project/procedure/product will fail and be of no
value if this requirement/defect/feature is not met/fixed/delivered.

_`.essential`: The project will not meet all of its goals / the product
will have serious problems / the defect will cause significant costs
to users.  Omitting this will require renegotiation with the client.
There has to be a valid documented reasons to omit.  Could delay
delivery.

_`.optional`: Not necessary for delivery.  Delivery should not be
delayed.  Would be of value to users.  Intended and worth assigning
resources to.  Causes costs to users.  Product is not as good without
it.

_`.nice`: Not necessary.  Good to have.  Would be of some value to
users (e.g. would create goodwill or satisfaction).  Not worth
assigning resources to, but shouldn't be forgotten.  Worth doing if it
comes cheaply while doing something else.  Causes minor annoyance and
has easy workarounds.


Comparison with other methods
=============================

_`.comparison`:

=============  ============  =====================
MPS            MoSCoW [1]_   [RFC-2119]_
=============  ============  =====================
`.critical`_   must have     MUST, REQUIRED, SHALL
`.essential`_  should have   SHOULD, RECOMMENDED
`.optional`_   could have    MAY, OPTIONAL
`.nice`_       would like    n/a
=============  ============  =====================

.. [1] [IIBA_2009]_ §6.1.5.2.  The MPS scheme compares best to the
       `*variant* MoSCoW analysis
       <https://en.wikipedia.org/wiki/MoSCoW_method#Variants>`_.  In
       the base MoSCoW method, "W" expresses a negative requirement
       or priority, meaning "won't have".


A. References
=============

.. [IIBA_2009]
   "A Guide to the Business Analysis Body of Knowledge (2 ed.).";
   Kevin Brennan (ed.);
   International Institute of Business Analysis;
   2009;
   `ISBN 978-0-9811292-1-1
   <https://en.wikipedia.org/wiki/Special:BookSources/978-0-9811292-1-1>`_.

.. [RB_1999-05-20]
   "Product Quality through Change Management";
   Richard Brooksby;
   Ravenbrook Limited;
   1999-05-20;
   <https://www.ravenbrook.com/doc/1999/05/20/pqtcm/>.

.. [RB_2000-05-05]
   "Requirements for defect tracking integration";
   Richard Brooksby;
   Ravenbrook Limited;
   2000-05-05;
   <https://info.ravenbrook.com/project/p4dti/doc/2000-05-05/reqs/#section-2.2>.

.. [RFC-2119] "Key words for use in RFCs to Indicate Requirement
	      Levels"; S. Bradner; Harvard University; 1997-03;
	      <https://www.ietf.org/rfc/inline-errata/rfc2119.html>
	      (with errata).


B. Document History
===================

==========  ====  ===
2023-03-12  RB_   Created in response to `GitHub issue #142`_
==========  ====  ===

.. _RB: mailto:rb@ravenbrook.com

.. _`GitHub issue #142`: https://github.com/Ravenbrook/mps/issues/142


C. Copyright and License
========================

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
