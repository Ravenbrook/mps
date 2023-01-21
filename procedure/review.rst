===================================
Memory Pool System review procedure
===================================

:tag: proc.review
:author: Richard Brooksby
:organization: Ravenbrook Limited
:date: 2023-01-19
:confidentiality: public
:copyright: See `C. Copyright and License`_
:readership: MPS developers, [incorporate roles. RB 2023-01-20]
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
  - Check against kpa.pr, [CMU/SEI-93-TR-025]_, p L3-97]


2. Purpose
----------

The purpose of the review procedure is:

1. find and correct major defects

2. prevent future defects

A defect is a way in which the work does not meet its requirements.

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

Each role has tasks during each phase.

[See also book.gilb93.proc.*.  RB 2023-01-20]

_`.role.leader`: The *leader* organises the review and ensures the
procedures are executed.  The leader is responsible for managing the
process in all respects for productive results.

_`.role.author`: The *author* wrote the work under review.

_`.role.checker`: A *checker* checks the work during review.  There
should be more than one checker, and the author can be a checker.

_`.role.editor`: The *editor* edits the work to correct defects found
by review.  This is usually the author.

_`.role.quality`: The *quality* [Is this right? RB 2023-01-20] edits
process to prevent future occurences of defects found by review.

_`.role.scribe`: The *scribe* ensures defects are recorded.

_`.role.chief`: [Chief Inspection Leader in book.gilb93.  Need to look
this up.  RB 2023-01-20]

_`.role.manager`: The *manager* ensures adequate resources are
assigned to review and that reviews are happening.  [Project Manager
in book.gilb93.  Need to look this up. RB 2023-01-20]


4. Phases
---------

_`.phase`: The following procedures are performed more-or-less in
order.

#. _`.phase.request`: `.role.author`_ requests that their work be
   reviewed.  [How?  Relate to GitHub non-draft pull requests.  RB
   2023-01-20]

#. _`.phase.entry`: `.role.author`_ and `.role.leader`_ perform
   `.entry`_.  The rest of the Inspection process is only
   entered when a specified set of entry criteria have been met.

#. _`.phase.planning`: `.role.leader`_ performs `.planning`_, selecting a
   set of source documentation, candidate documentation, checklists,
   rule sets, checking rates, people, roles, and logging meeting rates
   to ensure maximum productivity.

#. _`.phase.kickoff`: `.role.leader`_, `.role.checker`_, and sometimes the
   `.role.author`_ perform `.ko`_.

   The leader can elect to run a "kickoff" meeting prior to the
   checking.  Team improvement goals and corresponding strategies are
   adopted.  Any necessary instructions will be given.

#. _`.phase.check`: `.role.checker`_ perform `.check`_.

   The checking phase has a recommended time or rate, but checkers
   have instructions to deviate from that whenever individual
   availability, role, or situation dictates, in order to increase
   productivity.

   The objective of individual checking is to identify a maximum of
   unique major issues which no other checker will bring to the
   logging meeting.  To do this each checker should have at least one
   special "checking role".

#. _`.phase.log`: The `.role.leader`_, the `.role.scribe`_, and
   `.role.checker`_ perform `.log`_.

   The team concentrates on logging items at a rate of at least one
   per minute.  Items logged include potential defects (issues),
   improvement suggestions, and questions of intent to the author.
   The leader permits little other verbal meeting activity.  Meetings
   last as maximum of two hours at the known optimum rate.  If
   necessary, work must be chunked to avoid tiredness.  Optimum
   checking rate for the meeting is determined by the percentage of
   new issues identified in the logging meeting as well as the
   quantity of the documents.

#. _`.phase.brainstorm`: `.role.leader`_, `.role.scribe`_, `.role.checker`_,
   and usually the `.role.author`_ perform `.brainstorm`_.

   The followup is done by the `.role.leader`_, and make take place any
   time after the brainstorm meeting.  [What is the followup?  RB
   2023-01-20]

   Immediately after each logging meeting time is used to brainstorm
   the process causes of major defects, and to brainstorm improvements
   to remove these causes.  The meeting shall last no more than half
   an hour.  The objective is to maximize production of useful ideas
   and personal commitment to change within that time.

#. _`.phase.estimation`: `.role.leader`_, `.role.scribe`_, `.role.checker`_,
   and usually the `.role.author`_ spend a few minutes estimating how
   productive the review was, by:

   - estimating the cost of the review (mostly work hours)
   - projecting what the defects would cost if uncorrected
   - projecting what similar defects would cost if not prevented

   and `.role.leader`_ records this information.

#. _`.phase.edit`: `.role.editor`_ performs `.edit`_.

   Issue analysis and correction action is undertaken by an editor.
   Some written action must be taken on all logged issues -- if
   necessary by sending change requests to other authors.  The editor
   makes the final classification of issues into defects, and reports
   final defect metrics to the leader.  Edit also deals with
   improvements and can deal with "questions to the author".

#. _`.phase.quality`: The Quality [what?  RB 2023-01-20]

#. _`.phase.exit`: `.role.editor`_ and `.role.leader`_ perform `.exit`_.

   The leader shall determine that some appropriate written action has
   been taken on all logged issues.  The leader is not responsible for
   the correctness (the editor is).

   The leader determines whether the formal exit criteria have been
   met before signing off completion of the Inspection.  These include
   follow-up completed, metrics delivered, planned rates kept to, and
   level of remaining defects within acceptable bounds.


5. Procedures
-------------

5.1. Review Entry
.................

[Sourced from [MM_proc.review.entry] and needs updating.  RB
2023-01-21]

_`.entry`: [Placeholder.]

#. _`.entry.product`: Determine exactly what product document is
   involved, including revision.

#. _`.entry.draft`: Ensure that the product document is available in a
   suitable form and status (draft).

#. _`.entry.time`: Estimate the checking rate and time.  A single
   review should not have a checking time of more than one hour.

#. _`.entry.plan`: Plan when this review may take place and who should
   attend.  Check with attendees if appropriate.

#. _`.entry.record`: Create a review record for the product document
   as "review.<tag>.<revision>".  [Where?  Needs specific instructions
   for Git / GitHub.  RB 2023-01-20]

#. _`.entry.source`: Determine and record the source documents.

#. _`.entry.rule`: Determine and record the rule documents.

#. _`.entry.check`: Determine and record the checklists, including
   checking roles.

#. _`.entry.entry`: Determine and record the entry and exit criteria.

#. _`.entry.check-entry`: Check that the entry criteria hold.  Record
   any transgressions.  Determine whether the transgressions merit
   rejection at this stage.

#. _`.invite`: Invite the Checkers to the kickoff meeting.


5.2. Review Planning
....................

_`.planning`: [Placeholder.]


5.3. Review Kickoff
...................

[Sourcec from [MM_proc.review.ko] and needs updating.  RB 2023-01-21]

_`.ko`: [Placeholder.]


In Advance
~~~~~~~~~~

[This section could be moved to the planning phase.  RB 2023-01-21]

_`.ko.doc.prep`: In advance of the meeting, the leader ensures that checkers have 
access to the necessary documents, either by supplying them with physical 
copies, or by advising them of the documents in advance.

_`.ko.train.prep`: If any checkers are not familiar with formal review, the leader 
should ensure that they are briefed, and supplied with the relevant process 
documents.


At The Meeting
~~~~~~~~~~~~~~

_`.ko.record`: Times, objectives, and anything else appropriate should all be 
recorded in the review record.

_`.ko.doc.check`: In the meeting, the leader checks that all checkers have access to 
all necessary documents.

_`.ko.intro`: The leader may ask the author to prepare a short (one minute) 
introduction to the product document.

_`.ko.role`: The leader announces or negotiates any roles he wishes to assign, and 
ensures that checkers understand their assignments.

_`.ko.improve`: The leader announces any relevant metrics and negotiates objectives.

_`.ko.log`: The leader announces the time of the logging meeting.  This should 
normally be set at the estimate end of the kickoff meeting, plus the estimated 
checking time, plus a short tea-break.  It should not normally be delayed to 
another day.

_`.ko.remind`: The leader reminds checkers of the immediate objective of review (see 
process.review.goal.fix).

_`.ko.author`: The leader reminds the author that he can withdraw the document from 
review at any time.

_`.ko.train.check`: The leader checks that checkers are familiar with their tasks and 
solicits any questions or suggestions.


5.4. Review Checking
....................

_`.check`: [Placeholder.]


5.5. Review Logging
...................

_`.log`: [Placeholder.]


5.6. Review Brainstorm
......................

_`.brainstorm`: [Placeholder.]


5.7. Review Edit
................

_`.edit`: [Placeholder.]


5.8. Review Exit
................

[Sourced from [MM_proc.review.exit] and needs updating.  RB
2023-01-21]

_`.exit`: [Placeholder.]

_`.exit.request`: The editor requests the leader to exit the document.

_`.exit.check`: The leader checks that the document passes all
relevant exit criteria.  These should be indicated in review record.
 
_`.exit.check.fix`: If it doesn't pass all exit criteria, but it is
possible to fix it, he may either fix it himself, or return it to the
editor.

_`.exit.check.fail`: If the document cannot be made to pass exit (if,
say, there are two many estimated defects remaining), it may be passed
back to development, and reviewed subsequently.  The document remains
draft, and the review record becomes draft.  The reasons for failure
should be documented in the review record.

_`.exit.check.pass`: If it passes all criteria, the leader sets the
document status to "accepted" and the review record to "draft".  The
date of exit and any notes should be recorded in the review record.
The document is now suitable for release as appropriate.

_`.exit.inform`: The leader should inform all review participants and
some archived mailing list (such as "mm"), of the result of the
review, and any notes that seem appropriate.


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

.. [MM_proc.review.entry] "Procedure for review entry"; Gavin
			  Matthews; 1997-06-02; mminfo:proc.review.entry;
			  //info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mminfo/proc/review/entry/index.txt#1.

.. [MM_proc.review.exit] "Procedure for exiting a document from
			 review"; Gavin Matthews; 1997-06-12;
			 mminfo:proc.review.exit;
			 //info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mminfo/proc/review/exit/index.txt#1.

.. [MM_proc.review.ko] "Procedure for a review kickoff meeting"; Gavin
		       Matthews; 1997-06-12; mminfo:proc.review.ko;
		       //info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mminfo/proc/review/ko/index.txt#1.


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
