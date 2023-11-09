===============================
Rules for source code in Python
===============================

:Tag: rule.code.python
:Author: Gareth Rees
:Organization: Ravenbrook Limited
:Date: 2001-05-23
:Readership: Python programmers
:Confidentiality: public
:Copyright: See `C. Copyright and License`_


1. Introduction
===============

This document is a ruleset which can be applied to Python source code.
It is intended for use with an inspection procedure [Gilb-1995]_ but
can also be used for general guidance and review.

The intended readership is Ravenbrook staff, but these rules can used by
anyone.

This document is not confidential.


2. Rules
========

_`.compatible`: Write code that's compatible with Python version 1.5.2
and all later versions.  [We should update this to Python 3.  RB
2023-01-26]

_`.global`: Use global variables only for read-only data.

_`.instance`: Assign modifiable instance variables in a class's
``__init__`` method, not in the class definition at top level.

_`.joining`: Don't use a backslash to join two lines unless you have
to.

_`.unittest`: Use the Python unit test framework for your tests.


3. Justification and commentary
===============================

3.1. Compatibility
------------------

_`.compatible.just`:

It's very annoying to have to upgrade your Python
installation in order to run a program. Requiring the newest version
of Python could be a barrier to a customer (who already has a Python
installation) using a program.

Of course, this may not be possible, because you require a feature
that's only available in a newer version. If so, document this clearly
in the introduction to the module, package or product.

Main points to note:

#. Don't rely on nested environments (introduced in Python 2.1
   [Kuchling-2001a]_ `§2
   <https://web.archive.org/web/20010419075359/http://www.amk.ca/python/2.1/index.html#SECTION000300000000000000000>`__).
   Pass in the environment you need as arguments to the locally
   declared function or the ``lambda`` expression.

#. Don't use the augmented assignment operators ``+=``, ``-=``, ``*=``
   and so on (introduced in Python 2.0 [Kuchling-2000]_ `§6
   <https://web.archive.org/web/20010116011100/http://www.python.org/2.0/new-python.html#SECTION000700000000000000000>`__).
   Write ``a = a + b`` instead of ``a += b``.

#. Don't rely on garbage collection of cycles (introduced in Python
   2.0 [Kuchling-2000]_, `§8
   <https://web.archive.org/web/20010116011100/http://www.python.org/2.0/new-python.html#SECTION000900000000000000000>`__).
   Break the cycle explicitly.

#. Be careful with the division operator on integers; in Pythons before
   3.0 it returns the floor of the result; from Python 3.0 ``x/y`` will
   mean ``float(x)/float(y)`` (see [Kuchling-2001b]_ `§6
   <https://web.archive.org/web/20020604015808/http://www.amk.ca/python/2.2/index.html#SECTION000700000000000000000>`__).
   Use ``int(math.floor(x/y))`` if you want the old meaning, or
   ``float(x)/float(y)`` if you want the new meaning. (In a few cases it
   doesn't matter.)


3.2. Globals
------------

_`.global.just`:

Python's scope rules ([Python-2000-10-16]_, `§4.1
<https://web.archive.org/web/20040315042256/http://www.python.org/doc/2.0/ref/execframes.html>`__)
mean that a mention of variable in a function definition may refer
either to a local variable or a global variable, depending on whether
the variable is assigned to anywhere in the function definition,
unless you remember to specify ``global variable``. This is a source
of hard-to-discover defects.

Instead of using global variables, make classes and turn your global
variable into instance variables (but don't forget `.instance`_). This
also has the advantage of making it possible for several instances of
your code to run at the same time, thus meeting
`rule.code.adaptable`_.

.. _rule.code.adaptable: rule.code.rst#2-rules


3.3. Instances
--------------

_`.instance.just`:

Python's object instantiation rules ([Python-2000-10-16]_, `§7.6
<https://web.archive.org/web/20040315042256/http://www.python.org/doc/2.0/ref/class.html>`__)
mean that variables declared at top level in a class are shared
between all instances of the class. That's OK for non-modifiable
variables (for example, ``i = 1``) because an assignment to a class
variable actually creates an instance variable that shadows the class
variable. But for lists, dictionaries and other modifiable objects you
may be surprised when you find out that your list is shared between
all instances of the class.


3.4. Joining
------------

_`.joining.just`:

Backslashes don't cope with code reformatting. You can usually arrange
for lines to be join implicitly by adding extra parentheses.

Sometimes this can't be done, for example the ``raise`` and ``print``
statements sometimes need backslashes. But consider putting the long
expression in a variable.


3.5. Unit testing
-----------------

_`.unittest.just`:

The Python unit test framework [PyUnit]_ is the standard way of
developing sets of unit tests. (It is distributed with Python 2.1, but
needs to be downloaded and installed if you have an earlier Python.

Using PyUnit makes it easier to collect sets of tests into test suites
and to pick out individual tests and run them.


A. References
=============

.. [GDR-2001-05-25]
    "Rules for source code style";
    Gareth Rees;
    `Ravenbrook Limited`_;
    2001-05-25.

.. [Gilb-1988]
    "Principles of Software Engineering Management";
    Tom Gilb;
    `Addison-Wesley`_;
    1988;
    ISBN 0-201-19246-2.

.. [Gilb-1995]
    "Software Inspection";
    Tom Gilb, Dorothy Graham;
    `Addison-Wesley`_;
    1995;
    ISBN 0-201-63181-4.

.. [Kuchling-2000]
    "What's New in Python 2.0";
    `A M Kuchling <mailto:amk1@bigfoot.com>`__ and
    `Moshe Zadka <mailto:moshez@math.huji.ac.il>`_;
    2000;
    <https://web.archive.org/web/20010116011100/http://www.python.org/2.0/new-python.html>.

.. [Kuchling-2001a]
    "What's New in Python 2.1";
    `A M Kuchling <mailto:amk1@bigfoot.com>`__;
    2001;
    <https://web.archive.org/web/20010207043855/http://amk.ca/python/2.1/>.

.. [Kuchling-2001b]
    "What's New in Python 2.2";
    `A M Kuchling <mailto:amk1@bigfoot.com>`__;
    2001;
    <https://web.archive.org/web/20020604015808/http://www.amk.ca/python/2.2/index.html>.

.. [PyUnit]
    "PyUnit - a unit testing framework for Python";
    `Steve Purcell <mailto:stephen_purcell@yahoo.com>`_;
    <http://pyunit.sourceforge.net/>.

.. [Python-2000-10-16]
    "Python Reference Manual (Python 2.0)";
    `Guido van Rossum <mailto:python-docs@python.org>`_;
    2000-10-16;
    <https://web.archive.org/web/20080706160746/http://python.org/doc/2.0/ref/ref.html>.

.. [RB-2001-05-24]
    "Re: Rules for Python";
    `Richard Brooksby <mailto:rb@ravenbrook.com>`_;
    `Ravenbrook Limited`_;
    2001-05-24;
    <https://info.ravenbrook.com/mail/2001/05/24/12-34-31/0.txt>.

.. _`Addison-Wesley`: http://www.awl.com/


B. Document History
===================

==========  =====  ==================================================
2001-05-23  GDR_   Created based on outcome of informal inspection.
2001-05-25  GDR_   Moved style rule to [GDR-2001-05-25]_, following
                   the advice in [RB-2001-05-24]_.
2001-08-15  GDR_   Added warning about compatibility of division
                   operator between Python versions.
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
