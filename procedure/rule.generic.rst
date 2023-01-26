=======================
Rules for all documents
=======================

:Author: Richard Brooksby <rb@ravenbrook.com>
:Organization: Ravenbrook Limited
:Date: 1998-06-30
:Revision: $Id$
:Readership: anyone
:Confidentiality: public
:Copyright: Copyright 1998-2015 Ravenbrook Limited
:License: http://creativecommons.org/licenses/by-sa/4.0/


1. Introduction
---------------

This document is a set of rules which apply to any document.  The term
document covers almost any work product, including source code. The
rules are intended for use with an inspection procedure [Gilb-1995]_ but
can also be used for general guidance and review.


2. Rules
--------

_`.achieve`: A document must achieve (be consistent with) its purpose.

_`.automated`: It should be possible to process a document using automated tools.

_`.change`: It must be possible to retrieve a complete and justified history of changes to a document.

_`.clear`: A document must be clear to the intended readership.

_`.complete`: A document must be complete for its purpose.

_`.confidential`: A document must state whether it is confidential, and if
so, who can read it.

_`.ident`: Documents must be marked with a unique identifier which includes
the document revision, preferably a source control ID.

_`.note`: Notes or annotations which do not form part of the main document
must be clearly indicated.

_`.once`: A document must make statements once and thereafter refer to the
original statement.

_`.other`: Documents must be consistent with other documents.

_`.purpose`: The purpose of a document must be clear to the intended readership.

_`.readership`: The intended readership of a document must be clear to anyone.

_`.ref`: All statements in a document must be easily referenceable.

_`.ref-contents`: Each entry in the references section (conventionally
appendix A) should consist of title, author, company, publisher, date,
ISBN, and URL (or as many of these as are appropriate to the reference).

_`.ref-name`: A reference should be named after its author and date (if known).

_`.ref-sort`: References should be sorted in order by name.

_`.self`: Documents must be consistent with themselves.

_`.simple`: A document must be as simple as possible for its purpose. 
Simplicity is not the same as terseness.

_`.sources`: All statements shall refer to sources which justify them.

_`.standard`: A document must follow applicable standards.


3. Justification and commentary
-------------------------------

This ruleset is designed to tackle the main causes of defects:

1. poor communication,

2. poor justification,

3. inconsistency,

4. incomplete or lost information.

_`.justification-achieve`: Anything which does not achieve its purpose is
defective by definition.  This rule allows one to express this simple
fact by reference to a rule.

_`.justification-automated`: This means that documents should be
structured and formatted in a consistent and predictable way (which is
important for other reasons; see the `.self`_ and `.other`_ rules).

There are many reasons why documents may need to be processed by
automated tools.  For example: checking documents for errors in
formatting; checking the contents of one document against another it is
supposed to be consistent with; finding all documents that reference a
document (when the latter changes in some way); re-writing links when
part of a project is branched; editing using regular expressions or
editor macros.  Not all reasons can be anticipated in advance.

Automated checking and processing of documents is much more reliable
than doing the same by hand and is likely to result in fewer errors.

_`.justification-change`: The purpose of the `.change`_ rule is to make
sure that the revision histories of all documents can be understood.
This is especially important with source code, where tiny changes can
critically affect product functionality, but is also important for other
documents.  The word “justified” is important here.  The *reason* for
each change must be clear as well as the change itself.

If the intended readership does not have access to the source control
system, there must be a document history section (conventionally
appendix B).  Each entry in this section should consist of the date the
document was changed, the initials of the person who changed it, and a
short statement of what was changed.

If the intended readership have access to the source control system
containing the document, it is sufficient to give a source control
identifier for the document: see the `.ident`_ rule.  However, it is a
good idea to have a document history section anyway, because the
readership may be expanded later, and it is easier to maintain a
document history from the beginning than to fabricate one later based on
the source control history.

_`.justification-clear`: The purpose of a document is communication, and
if it is not clear to its intended readership then it is not achieving
that purpose.  It is essential that the communication be effective, or
misunderstandings will happen and defects will result.  This is perhaps
the most common source of defects.

**Note**: If you are in the intended readership of a document and it is
not clear to you then it is *not clear* and it is breaking this rule. 
You must not allow yourself to be persuaded (especially verbally) that
it is clear after all.

_`.justification-complete`: An incomplete document may be adequate to get
basic ideas across, but “the devil is in the details”.  Defects often
arise because of incomplete information.

Incomplete source code (failing to cover all cases, for example) is
usually defective.

_`.justification-confidential`: Ravenbrook staff work on a mixture of
confidential and publicly available documents.  It is important not to
confuse the former with the latter, otherwise private material could be
accidentally be made available to the public.

_`.justification-ident`: The `.ident`_ rule allows found documents to be
understood out of context.

Putting a document under source control and assigning it a source
control ID (usually using the “keyword expansion” feature of the source
control system) automatically achieves the `.ident`_ rule but and also
most of the `.change`_ rule.  The information system ensures that source
control IDs are easily translated to URLs which allow the document to be
retrieved from the information server.

_`.justification-note`: Notes allow authors to add incidental information
to documents.  This is often useful, but it is important to separate the
incidental information from the main part of the document so that it can
be understood to be incidental.

[My recommended practice for notes, by the way, is to use square
brackets and to sign your name and put the date at the end.  RB
1998-06-30]

**Important**: Do not confuse this kind of “note” with source code
“comments” (for which “comment” is a misnomer).  A source code comment
is a part of the document and must obey all the normal document rules.

_`.justification-once`: Documents which contain redundancy are fragile:
it is easy to make them inconsistent when changed, introducing defects
which are hard to track down.  Redundancy should be avoided for this
reason, and any redundancy or dependency must be made very clear by
cross-referencing.

_`.justification-other`: This is a very powerful rule when combined with
the `.sources`_ rule.  Since every statement must be backed up by
sources, this rule allows one to check that the statement is in fact
consistent with those sources, and justified by them.  Thus the
connection between customer needs, requirements, specification, changes,
and product is checked step-by-step.

_`.justification-purpose`: If the purpose of a document is not clear then
it is not possible to check whether the document achieves its purpose
(see the `.achieve`_ rule).

Note that this rule does not require the purpose to be explicitly
stated, but it must be clear to the entire readership.  Usually it
should be stated.

_`.justification-readership`: The main purpose of this rule is to support
the `.clear`_ rule.  Without it, “clarity” cannot be defined.

The other purpose of this rule is to help people deal with “found”
documents.  Since anyone can identify the readership they know who to go
to for an interpretation of the document.

_`.justification-ref`: Statements must be easily referenced to support
cross-referencing from other documents (see the `.sources`_ rule) and
therefore checking for consistency between documents (see the `.other`_
rule).  Inconsistency between separate documents is a major source of
defects.

Similarly, statements must be easily referenced to support the `.once`_
rule, since self-inconsistency is another important source of defects.

_`.justification-ref-contents`: This is a specialization of the
`.sources`_ rule.

_`.justification-ref-name`: Dates must be in standard format [ISO-8601]_. 
Use as much of the date as you know.

For authors who are Ravenbrook staff, use their initials, for example,
[RB-1998-06-30], for consistency with Ravenbrook convention in e-mail
and messaging (`.other`_).  For other authors, use the surname, for
example [Gilb-1995], for consistency with general convention.
Distinguish documents written by the same author on the same date with
letters after the date, for example [RB-1998-06-30a].  Where you don’t
know the actual author, you can use the company, for example
[Perforce-2001-04-13], or make up a descriptive reference, for example
[XHTML-1.0].

_`.justification-ref-sort`: Sorting the references by name makes it
possible to find the reference you’re looking for.

_`.justification-self`: Self-inconsistency almost always indicates a
defect, because it indicates that the author (or authors) are not
communicating correctly.

Inconsistency is also a needless source of complexity.  If a document
does something one way, and then a similar thing a different way, then
it is not simple enough.

_`.justification-simple`:

    “Everything should be made as simple as possible, but no simpler.”
    (after Albert Einstein)

Complexity is a source of defects.  Something which is complex is hard
to understand, and therefore we can be less sure that it meets its
requirements.  The quality of complex things is therefore almost
inevitably lower than that of simple things.

Simple documents are easier to understand, maintain, and adapt.
Simplicity therefore reduces cost as well as increasing quality.

Software is complex enough without making it any more complex. Our
customer’s requirements are also complex and contradictory. We must
therefore combat complexity at every turn, or it will overwhelm us and
we will lose.

_`.justification-sources`: The main purpose of this rule, combined with
the `.other`_ rule, is to ensure that decisions are justified in terms
of customer needs.  This improves quality by directing all decisions
towards customer need.

The secondary (but still very important) purpose of this rule is to make
it possible to understand the document in the future when we have
forgotten its connections to other documents.  This makes it possible to
maintain and adapt the document, and also makes it possible to detect
when the document is out of date with respect to other changes (another
big source of defects).

The source documents of source code are often issue or change documents
which caused that code to be the way it is.

The sources for a document should be listed in a references section.


_`.justification-standard`: Following applicable standards helps a
document to follow the `.self`_, `.other`_, and `.automated`_ rules.

In particular:

1. Dates and times should follow [ISO-8601]_. Write “2001-02-03”, not
“03/02/01”: the latter also means 1901-03-02, 2001-03-02, and other
dates.  Write “09:45”, not “9:45”: the latter also means 21:45 (when
“pm” is understood).

2. Currencies should follow [ISO-4217]_.  Write “GBP”, not “£”: the
latter isn’t represented in ASCII (and so may be transmitted incorrectly
in e-mail).  Write “USD”, not “$”: the latter is used for many
currencies, for example CAD, HKD, AUD, and NZD.


A. References
-------------

.. [Gilb-1988]
    “Principles of Software Engineering Management”;
    Tom Gilb;
    Addison-Wesley_;
    1988;
    ISBN 0-201-19246-2.

.. [Gilb-1995]
    “Software Inspection”;
    Tom Gilb, Dorothy Graham;
    Addison-Wesley_;
    1995;
    ISBN 0-201-63181-4.

.. [ISO-4217]
    “ISO 4217:1995 Codes for the representation of currencies and funds”;
    ISO_;
    1995.

.. [ISO-8601]
    “ISO 8601:2000 Data elements and interchange formats -- Information
    interchange -- Representation of dates and times”;
    ISO_;
    1988-06-15.

.. [RB-1998-06-30]
    “Generic Ruleset”;
    `Richard Brooksby`_;
    Ravenbrook_;
    1998-06-30.

.. _`Addison-Wesley`: http://www.awl.com/
.. _`ISO`: http://www.iso.ch/
.. _`Richard Brooksby`: mailto:rb@ravenbrook.com
.. _Ravenbrook: https://www.ravenbrook.com/


B. Document History
-------------------

- 2001-04-22 GDR_ Created based on [RB-1998-06-30]_.

- 2001-06-07 GDR_ Added `.standard`_ rule.

- 2001-07-11 GDR_ Moved rules ref-contents, ref-name and ref-sort from
  XHTML ruleset because they apply to all documents, not just XHTML
  documents.

- 2015-12-15 RB_ Converted to ReStructuredText and released under
  Creative Commons license.

.. _GDR: mailto:gdr@ravenbrook.com
.. _RB: mailto:rb@ravenbrook.com
