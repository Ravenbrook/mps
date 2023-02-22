.. mode: -*- rst -*-

Memory Pool System Project Plan
===============================

:Author: Gareth Rees
:Date: 2014-05-16
:Status: live
:Revision: $Id$
:Copyright: See `Copyright and License`_.


Introduction
------------

This is the plan for development of the Memory Pool System.

This document will be modified as the project progresses.

The readership of this document is anyone interested in the project.

This document is not confidential.


Future versions
---------------

Version 1.116
.............

Better measurement and visualization of space and time performance.

Improvements to performance and tunability via separation of concerns
(see [GDR_2014-05-15]_).

* job003783_ (ChainCondemnAuto condemns too many generations)
* job003796_ (White segment lookup is slow)
* job003797_ (AMC space is lost to pinning)
* job003799_ (Incrementally collecting the nursery may be a waste)

.. _job003783: https://www.ravenbrook.com/project/mps/issue/job003783/
.. _job003796: https://www.ravenbrook.com/project/mps/issue/job003796/
.. _job003797: https://www.ravenbrook.com/project/mps/issue/job003797/
.. _job003799: https://www.ravenbrook.com/project/mps/issue/job003799/

Improvements to flexibility by removing special cases and
introducing abstractions:

* job003765_ (Pools use GCSeg when they are not GC pools)
* job003838_ (MVT uses segments unnecessarily)

.. _job003765: https://www.ravenbrook.com/project/mps/issue/job003765/
.. _job003838: https://www.ravenbrook.com/project/mps/issue/job003838/

Improvements to usability:

* job003794_ (Hard to predict the mortality in a generation)

.. _job003794: https://www.ravenbrook.com/project/mps/issue/job003794/

Improvements to testing:

* job003839_ (No benchmarks for space performance)

.. _job003839: https://www.ravenbrook.com/project/mps/issue/job003839/


Past versions
-------------

Version 1.115
.............

Gain client control over pause times (see [GDR_2014-05-14]_ and [GDR_2014-05-15]_).

* job003539_ (MPS pause times are not well regulated)

.. _job003539: https://www.ravenbrook.com/project/mps/issue/job003539/


Version 1.114
.............

Improvements to performance and tunability via separation of concerns
(see [GDR_2014-05-15]_).

* job003509_ (MVFF uses segments unnecessarily)
* job003554_ (MPS slows down considerably when arena is extended)
* job003701_ (MVSpanAlloc shows up in GC profiles)
* job003812_ (MVAlloc taking significant CPU in profiles)
* job003823_ (No control over constant factor in tract management)
* job003824_ (No control over the constant factor in AMC segment overhead)

.. _job003509: https://www.ravenbrook.com/project/mps/issue/job003509/
.. _job003554: https://www.ravenbrook.com/project/mps/issue/job003554/
.. _job003701: https://www.ravenbrook.com/project/mps/issue/job003701/
.. _job003812: https://www.ravenbrook.com/project/mps/issue/job003812/
.. _job003823: https://www.ravenbrook.com/project/mps/issue/job003823/
.. _job003824: https://www.ravenbrook.com/project/mps/issue/job003824/

Improvements to flexibility by removing special cases and
introducing abstractions:

* job003684_ (Too hard to swap out address range managers)
* job003685_ (No encapsulation of CBS-failing-over-to-Freelist pattern)
* job003745_ (AWL alignment is not configurable)
* job003748_ (Alignment requirements for manual classes are needlessly strict)
* job003787_ (No systematic interface to size of pools)

.. _job003684: https://www.ravenbrook.com/project/mps/issue/job003684/
.. _job003685: https://www.ravenbrook.com/project/mps/issue/job003685/
.. _job003745: https://www.ravenbrook.com/project/mps/issue/job003745/
.. _job003748: https://www.ravenbrook.com/project/mps/issue/job003748/
.. _job003787: https://www.ravenbrook.com/project/mps/issue/job003787/

Improvements to robustness:

* job001549_ (Assertion failure ``!AMS_IS_INVALID_COLOUR``)
* job003359_ (Ambiguous interior pointers do not keep objects alive)
* job003496_ (Assertion failure in ``mps_arena_roots_walk``)
* job003751_ (MVFF debug does not work with large objects)
* job003771_ (AMS with default args never gets collected)
* job003772_ (AWL doesn't provoke collections)
* job003773_ (Objects in LO pools are not all finalized)

.. _job001549: https://www.ravenbrook.com/project/mps/issue/job001549/
.. _job003359: https://www.ravenbrook.com/project/mps/issue/job003359/
.. _job003496: https://www.ravenbrook.com/project/mps/issue/job003496/
.. _job003751: https://www.ravenbrook.com/project/mps/issue/job003751/
.. _job003771: https://www.ravenbrook.com/project/mps/issue/job003771/
.. _job003772: https://www.ravenbrook.com/project/mps/issue/job003772/
.. _job003773: https://www.ravenbrook.com/project/mps/issue/job003773/

Improvements to testing:

* job003659_ (Too hard to maintain the test suite)
* job003716_ ("ANSI" platform is not regularly tested)
* Static analysis using Coverity_.
* Test coverage up to 84%.

.. _job003659: https://www.ravenbrook.com/project/mps/issue/job003659/
.. _job003716: https://www.ravenbrook.com/project/mps/issue/job003716/
.. _Coverity: http://www.coverity.com/


References
----------

.. [GDR_2014-05-14] "MPS strategic direction proposals"; Gareth Rees;
   Ravenbrook Limited; 2014-05-14;
   <https://info.ravenbrook.com/mail/2014/05/14/16-14-34/0/>.

.. [GDR_2014-05-15] "More MPS strategy"; Gareth Rees;
   Ravenbrook Limited; 2014-05-15;
   <https://info.ravenbrook.com/mail/2014/05/15/19-19-13/0/>.



Copyright and License
---------------------

Copyright © 2013-2014 Ravenbrook Limited. All rights reserved.
<https://www.ravenbrook.com/>. This is an open source license. Contact
Ravenbrook for commercial licensing options.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

#. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

#. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

#. Redistributions in any form must be accompanied by information on how
   to obtain complete source code for this software and any
   accompanying software that uses this software.  The source code must
   either be included in the distribution or be available for no more than
   the cost of distribution plus a nominal fee, and must be freely
   redistributable under reasonable conditions.  For an executable file,
   complete source code means the source code for all modules it contains.
   It does not include source code for modules or files that typically
   accompany the major components of the operating system on which the
   executable file runs.

**This software is provided by the copyright holders and contributors
"as is" and any express or implied warranties, including, but not
limited to, the implied warranties of merchantability, fitness for a
particular purpose, or non-infringement, are disclaimed.  In no event
shall the copyright holders and contributors be liable for any direct,
indirect, incidental, special, exemplary, or consequential damages
(including, but not limited to, procurement of substitute goods or
services; loss of use, data, or profits; or business interruption)
however caused and on any theory of liability, whether in contract,
strict liability, or tort (including negligence or otherwise) arising in
any way out of the use of this software, even if advised of the
possibility of such damage.**
