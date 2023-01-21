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

This is the procedure for reviewing changes to the MPS in order to
`prevent defects <2. Purpose>`_.

This is a placeholder while we arrange to bring in documents from
outside the MPS Git repository and tree.  For background, see `GitHub
issue #95 <https://github.com/Ravenbrook/mps/issues/95>`_.

Since this document was created as part of `a project to migrate from
Perforce to Git (and GitHub)
<https://github.com/orgs/Ravenbrook/projects/1>`_, this procedure will
give specifics on conducting review via GitHub.  But the process is in
no way specific to GitHub, Git, Perforce, or any other tool.

This procedure was created by locating `review process documents from
the Harlequin MM Group <A. References>`_, incorporating the wisdom
from thousands of hours of reviews.  The text was then edited to
bring it up to date with current terms and tools.

This procedure may seem overwhelming at first, but it can be executed
quickly [how quickly? kpa.qpm! RB 2023-01-21] once you have learned it
[and how long does that take?  RB 2023-01-21].  [Insert example times
here for different kinds of work.  RB 2023-01-21]

Hundreds of reviews have been conducted using it in the past, and
every single one was profitable in terms of preventing defects versus
cost of review [citation needed. RB 2023-01-21].  This is in part
because *defects in garbage collectors are extremely expensive to
fix*.

The process is largely derived from "Software Inspection" [Gilb_93]_.
It was developed over years by Gavin Matthews of the Harlequin Memory
Management Group, who was eventually trained in the art.

The process is an example of "Peer Review" (kpa.pr), a key process
area of level 3 of the Capability Maturity Model [CMU/SEI-93-TR-025]_,
but also contributes a great deal to:

- Defect Prevention (kpa.dp, level 5)
- Process Change Management (kpa.pcm, level 5)
- Quantitive Process Management (kpa.qpm, level 4)

[Notes for inclusion:
  - Review is not just (or even mostly) looking at diffs, though
    GitHub encourages this idea.
  - History of MPS review.
  - Why review is so important to the MPS.
  - Review is not just for code.
  - Check against book.gilb93.handbook [Gilb_93]_.
  - Check against kpa.pr, [CMU/SEI-93-TR-025]_, p L3-97
  - proc.review.ref.]


2. Purpose
----------

The purpose of the review procedure is:

1. _`.goal.fix`: find and correct major defects

2. _`.goal.prevent`: prevent future defects

A defect is a way in which the work does not meet its requirements.

As with any procedure, you can vary this one to meet this purpose, but
you should probably read section [Insert reference to Rationale here.
RB 2023-01-20].


3. Review Rôles
---------------

[Reconstructed from memory and common sense, and then sourced from
[MM_process.review]_, but needs updating.  RB 2023-01-20]

Reviews must involve more than one person.  There must be at least one
checker who is not the author.

Each person can takes on a number of rôles.

Every role must be assigned to someone.

Each role has tasks during each phase.

[See also book.gilb93.proc.*.  RB 2023-01-20]

_`.role.leader`: The *leader* organises the review and ensures the
procedures are executed.  The leader is responsible for managing the
process in all respects for productive results.  The leader is the
person responsible for organizing and planning review.  They need not
be distinct from the `.role.author`_.  It is helpful if the leader has
received special instruction in review or inspection.

_`.role.author`: The *author* wrote the work under review.  The author
is the person primarily responsible for the production of the product
document (see `.doc.product`_).

_`.role.checker`: A *checker* checks the work during review.  There
should be more than one checker, and the author can be a checker.  A
checker is any person who participates in the checking of product
documents, and process brainstorming.  All review attendees are
usually checkers.  Checkers may be asked by the leader to check with
certain *checking roles* in mind; this is to increase coverage and
reduce duplication of issues.  [Checking roles are available in
mminfo:role.check.{backwards,clarity,consistency,convention,correctness,source}
and these need to be referencable from here, possibly included in this
document. RB 2023-01-21]

_`.role.editor`: The *editor* edits the work to correct defects found
by review.  This is usually the author.  The editor is the person
responsible for acting on issues raised, and bringing the product
document to review exit.  They are usually the author.

_`.role.quality`: The *quality* [Is this right? RB 2023-01-20] edits
process to prevent future occurences of defects found by review.

_`.role.scribe`: The *scribe* ensures defects are recorded.  The
scribe is the person who records information during review meetings.
They are usually the same person as `.role.leader`_.

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

[Sourced from [MM_proc.review.entry]_ and needs updating.  RB
2023-01-21]

_`.entry`: [Placeholder.]

_`.entry.product`: Determine exactly what product document is
involved, including revision.

_`.entry.draft`: Ensure that the product document is available in a
suitable form and status (draft).

_`.entry.time`: Estimate the checking rate and time.  A single review
should not have a checking time of more than one hour.

_`.entry.plan`: Plan when this review may take place and who should
attend.  Check with attendees if appropriate.

_`.entry.record`: Create a review record for the product document as
"review.<tag>.<revision>".  [Where?  Needs specific instructions for
Git / GitHub.  RB 2023-01-20]

_`.entry.source`: Determine and record the source documents.

_`.entry.rule`: Determine and record the rule documents.

_`.entry.check`: Determine and record the checklists, including
checking roles.

_`.entry.entry`: Determine and record the entry and exit criteria.
[These are available in mminfo:rule.entry.* and
mminfo:rule.exit.universal, and these need to be referencable from
here, probably in their own documents.  RB 2023-01-21]

_`.entry.check-entry`: Check that the entry criteria hold.  Record any
transgressions.  Determine whether the transgressions merit rejection
at this stage.

_`.invite`: Invite the Checkers to the kickoff meeting.


5.2. Review Planning
....................

_`.planning`: [Placeholder.]

[It seems that planning was folded into `.entry`_ in the MM Group.
Consider.  RB 2023-01-21]


5.3. Review Kickoff
...................

[Sourced from [MM_proc.review.ko]_ and needs updating.  RB 2023-01-21]

_`.ko`: [Placeholder.]


5.3.1. In Advance
~~~~~~~~~~~~~~~~~

[This section could be moved to the planning phase.  RB 2023-01-21]

_`.ko.doc.prep`: In advance of the meeting, the leader ensures that checkers have 
access to the necessary documents, either by supplying them with physical 
copies, or by advising them of the documents in advance.

_`.ko.train.prep`: If any checkers are not familiar with formal review, the leader 
should ensure that they are briefed, and supplied with the relevant process 
documents.


5.3.2. At The Meeting
~~~~~~~~~~~~~~~~~~~~~

_`.ko.record`: Times, objectives, and anything else appropriate should all be 
recorded in the review record.

_`.ko.doc.check`: In the meeting, the leader checks that all checkers have access to 
all necessary documents.

_`.ko.intro`: The leader may ask the author to prepare a short (one minute) 
introduction to the product document.

_`.ko.role`: The leader announces or negotiates any checking roles he
wishes to assign, and ensures that checkers understand their
assignments.

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

[Sourced from [MM_proc.review.check]_ and needs updating.  RB 2023-01-21]

_`.check`: [Placeholder.]

[Note: not all issues are local to a line.  RB 2023-01-21]


5.4.1. Start
~~~~~~~~~~~~

_`.check.doc`: Ensure that you have all the relevant documents.

_`.check.ask`: Ask the review leader if you have any questions about
checking procedure.


5.4.2. Checking
~~~~~~~~~~~~~~~

_`.check.source`: First, read any source documents.  Review is not
directed at finding defects in source documents, but any found are a
bonus.  They will be improvement suggestions (see class.imp [To what
does this refer?  RB 2023-01-21]).  Do not waste too much time finding
defects in source documents.

_`.check.rule`: Ensure that you are familiar with all rule sets or
check lists.

_`.check.role`: Ensure that you know and keep in mind the roles you
have been assigned.

_`.check.product`: Read through the product document (or documents) in
the order specified.  Remember to read the product documents in
reverse order if you were assigned a backwards checking role during
`.ko.role`_ (see role.check.backwards [Needs importing.  RB
2023-01-21]).

_`.check.major`: Concentrate on finding major issues (see
guide.review.class.major [Needs importing.  RB 2023-01-21]); this is
of primary importance.

_`.check.max`: Find as many issues as possible to help the author.

_`.check.note`: Note all issues; you need not log them later.

_`.check.rough`: Your log can be rough; concentrate on finding issues.

_`.check.trouble`: Consult the leader if you have any questions, or if
you are finding too many or too few issues.

_`.check.class`: Classify each defect you find according to
guide.review.class [Needs importing.  RB 2023-01-21].


5.4.3. End
~~~~~~~~~~

_`.check.record`: At the end of checking, record (for each product
document):

- How many defects were found, by class (see `.check.class`_);

- How long was actually spent;

- How much of the product document was actually checked;

- Any problems encountered.


5.5. Review Logging
...................

[Sourced from [MM_proc.review.log]_ and needs updating.  RB 2023-01-21]

_`.log`: [Placeholder.]

_`.log.just`: The main reason for having joint logging sessions is so
that new issues are found.


5.5.1. During The Meeting
~~~~~~~~~~~~~~~~~~~~~~~~~

_`.log.record`: All information gathered should be recorded in the
review log.  This may be deferred if the meeting is mediated by a
logged medium, such as IRC.

_`.log.metrics`: Gather individual metrics of:

- Issue counts by class;

- Time spent checking;

- Amount of product document actually checked.

_`.log.author`: The leader reminds the author that he may remove
documents from review at any time.

_`.log.decide`: The leader, in consultation with the author and
editor, decides whether it is worth holding continuing with the
logging meeting.  [Using what criteria?  We've never actually done
this.  GavinM 1997-06-12] In particular, see exit.universal.rates [To
what does this refer?  RB 2023-01-21].

_`.log.scribe`: Assign a scribe (usually the leader), and ensure the
editor will be happy with the readability of the log.

_`.log.explain`: The leader explains the order in which issues will be
logged, and ensures everyone understand this.  He also explains the
desired form of issues, namely:

- Location;

- Class, including "New" (N) if the issue was discovered during
  logging (see guide.review.class [Needs importing.  RB 2023-01-21]);

- Description of issue, concentrating on how it breaks a rule, rather
  than on possible solutions, naming the rule or checklist question,
  if possible.

_`.log.dup`: The leader should also explain that checkers should avoid
logging issues that have are duplicates of ones already logged, ut
that if in doubt, they should log.

_`.log.slow`: Issues are logged sufficienly slowly that all checkers
can examine each issue.  This is so that checkers can find new issues.

_`.log.order`: Unless instructed otherwise, checkers should try to
list their issues in forwards document order.  This makes life easier
for other checkers and the editor.

_`.log.fast`: Logging should more fairly brisky, however, and the
leader should be firm in discouraging discussion of:

- Whether issues are genuine defects;

- How a defect may be resolved;

- The review process (other than to answer questions);

- The answers to questions logged.

[And encouraging the search for more defects, see `.log.just`_.
RB 2023-01-21]

[ There has been much experimentation with the order of logging, but
this represents current best practice.  GavinM 1997-06-12 ]

_`.log.major`: The leader calls upon all checkers, one by one, to list
their major issues (see guide.review.class.major [Needs importing.  RB
2023-01-21]), preferable in order of their occurance in the product
document.  He may chunk the product document and go round the checkers
several times, but this is unusal.

_`.log.decide.non-major`: The leader may decide not to log all minor
issues (see guide.review.class.minor [Needs importing.  RB
2023-01-21]).  He should announce that each checker should offer some
number, or fraction.  Other issues may be logged in writing.

_`.log.non-major`: The leader takes all checkers through the product
document in order, at each stage:

- Announcing the section being looked at;

- Asking who has issues in this section;

- Requesting issues from checkers.  [This may be unnecessary if using
  an asynchronous medium, such as IRC.  GavinM 1997-06-12]

Note that improvement suggestions arising from specific parts of the
product document can be logged at this stage.

_`.log.general`: The leader then requests, by checker, any general or
new issues not already logged.

_`.log.brainstorm`: The leader negotiates a time for the process
brainstorm.  This will normally be a tea-break (10-15 minutes) after
the end of the logging meeting.


5.5.2. After The Meeting
~~~~~~~~~~~~~~~~~~~~~~~~

_`.log.inform`: The reviewed document is now ready for edit (see proc.review.edit).  
The review leader should inform the editor of this by mail.


5.6. Review Brainstorm
......................

[Sourced from [MM_proc.review.brainstorm]_ and needs updating.  RB
2023-01-21]

_`.brainstorm`: [Placeholder.]

_`.brainstorm.just`: The purpose of holding a process brainstorm
meeting is to meet the second goal of review (see
process.review.goal.prevent [Needs importing.  RB 2023-01-21]) by
finding ways to prevent the reoccurance of defects.  This closes the
process improvement loop.


5.6.1. In Advance
~~~~~~~~~~~~~~~~~

_`.brainstorm.choose`: The leader chooses 3-6 major defects or groups
of major defects (see guide.review.class.major [Needs importing.  RB
2023-01-21]) found in review.  They makes this choice based on their
importance and his own experience of which defects can be most
profitably attacked.


5.6.2. In The Meeting
~~~~~~~~~~~~~~~~~~~~~

_`.brainstorm.time`: The process brainstorm should last no more than
around 30 minutes.

_`.brainstorm.record`: The brainstorm should be recorded in the review
log as best as the scribe may.  This may be deferred if the process
brainstorm takes place by some logged medium, such as IRC.

_`.brainstorm.remind`: The leader reminds participants that their
purpose is to find process improvements that would have prevented
major defects from occurring.

_`.brainstorm.raise`: The leader raises each issue in turn, reminding
participants of the issue, and asking how it happenned and what could
have prevented it.

_`.brainstorm.disc`: The participants should discuss each defect for
no more than about five minutes.  They should focus on how the defect
arose, and what improvement could prevent it.  The leader should be
firm in curtailing discussion of how the defect can be fixed.

_`.brainstorm.proc`: If time permits, the leader may solicit
criticisms of the review process and apply `.brainstorm.disc`_ to
them.


5.6.3. After The Meeting
~~~~~~~~~~~~~~~~~~~~~~~~

_`.brainstorm.act`: The review leader should derive requests and
solution suggestions for the process product from the record, and
should note these in the review record where appropriate.  [This needs
to be made more specific.  RB 2023-01-21]


5.7. Review Edit
................

[Sourced from [MM_guide.review.edit]_ and needs updating.  RB 2023-01-21]

_`.edit`: [Placeholder.]

_`.edit.log`: The log should be placed in the edit section of the
review document.  The review document for a document of tag <tag> and
revision <revision> will be review.<tag>.<revision>.

_`.edit.order`: The log should be in numerical order, one issue per line.


Edit comments
~~~~~~~~~~~~~

_`.edit.edit-comments`: The following describes the format of edit
comments for each issue, indicating the action taken.  See
guide.review.class for issue classification.


Major Issues
............

_`.edit.major`: Major issues should receive one of the following
responses:

_`.edit.major.reject`: "Reject: <reason>"

  reject the issue with a reason why it is not a valid issue.

_`.edit.major.comment`: "Comment: <reason>"

  it is a valid issue, but merely add a comment to the document, the
  reason states why it cannot be fixed at this time.  Note that this
  is not the same as fixing a defect in a comment.

_`.edit.major.fix`: "Fix: <detail>"

  fix the defect and give some indication of how.

_`.edit.major.raise`: "Raise: <tag>"

  escalate the defect, usually by creating a request in MM Evolution.

_`.edit.major.other`: If a major defect results in a change to another document, that 
document's tag must be quoted.


Minor Issues
............

_`.edit.minor`: Minor issues should receive one of the following
responses:

_`.edit.minor.reject`: "Reject: <reason>"

  reject is issue with a reason why it is  not a valid issue.

_`.edit.minor.forget`: "Forget: <reason>"

  it is a valid issue but is not worth taking any action over.
  [Should we have this?]

_`.edit.minor.comment`: "Comment: <reason>"

  it is a valid issue, but merely add a comment to the document, the
  reason states why it cannot be fixed at this time.  Note that this
  is not the same as fixing a defect in a comment.

_`.edit.minor.fix`: "Fix: <detail>"

  fix the defect and give an indication of how; the detail is optional
  where the fix is obvious.

_`.edit.minor.raise`: "Raise: <tag>"

  escalate the defect, usually by creating a request in MM Evolution.

_`.edit.minor.other`: If a minor defect results in a change to another document, that 
document's tag must be quoted.


Comments
........

_`.edit.comment`: Comments on the product document should receive one of the following 
responses:

_`.edit.comment.reject`: "Reject: <reason>"

  reject the comment with a reason why it is invalid.

_`.edit.comment.forget`: "Forget: <reason>"

  it is a valid comment, but isn't worth taking any action over.  The
  reason is optional.

_`.edit.comment.comment`: "Comment: <detail>"

  a comment has been added to the document.  The detail is optional.

_`.edit.comment.fix`: "Fix: <detail>"

  the comment has resulted in a change to the product document.

_`.edit.comment.other`: If a comment results in a change to another
document, that document's tag must be quoted.


Questions To The Author
.......................

_`.edit.question`: Questions to the author should receive one of the following 
responses:

_`.edit.question.mail`: "Mail: <tag>.."

  the question is answered in the specified mail message(s).

_`.edit.question.raise`: "Raise: <tag>"

  the question has been escalated to the specified document, usually a
  request in MM Evolution.


Improvement Suggestions
.......................

[This clashes with the idea of a separate `.role.quality`_.  RB
2023-01-21]

_`.edit.improve`: Improvement suggestions should receive one of the
following responses:

_`.edit.improve.edit`: "Edit: <tag> <detail>"

  edit of another document.  The detail is optional if it is obvious.

_`.edit.improve.pass`: "Pass: <person>"

  passed to another person, who has accepted it.

_`.edit.improve.raise`: "Raise: <tag>"

  elevated, usually to a request in MM Evolution.

_`.edit.improve.reject`: "Reject: <reason>"

  rejected because it is not a valid issue.

_`.edit.improve.forget`: "Forget: <reason>"

  it is a valid issue, but is not worth taking any action
  over. [Should we have this?]


Calculations
~~~~~~~~~~~~

[This section was found in guide.review.edit but seems out of place.
RB 2021-01-21]

_`.edit.manpower-used`: The manpower used is the time for entry,
kickoff, checking, logging, brainstorm, edit, and exit.  Kickoff,
checking, logging and brainstorm must be multiplies by the number of
checkers.  Entry and kickoff may be assigned to another document
reviewed at the same time.

_`.edit.manpower-saved`: The default calculation is the number of
major defects found and fixed, multiplies by 10 man-hours.  This
represent the cost of a major defect found by QC.  If the defect would
have reached customers, the estimate should be 100 man-hours.  A
better estimate can be made, with justification.

_`.edit.defects-remaining`: The calculation of defects remaining
should use the estimate <major defects found>/<number of pages>.  The
obvious adjustment must be made for sampling.  The number of
unresolved major issues (raised) should be added.  [In an ideal world,
I believe we should know what proportion of major defects we find, and
use that.  Perhaps we could use 75%? - GavinM]


5.8. Review Exit
................

[Sourced from [MM_proc.review.exit]_ and needs updating.  RB
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


6. Documents
------------

[Sourced from [MM_process.review]_ and needs updating.  RB 2023-01-21]

_`.doc`: The review process involves a lot of documents.  This is a
brief explanation of what they are:

_`.doc.source`: Source document

  A document from which the product document is derived.  Note that
  this is nothing to do with source code.

_`.doc.product`: Product document

  The document developed from the source documents, and offered for
  review.  The work under review.  The changes under review.  The word
  product.

_`.doc.record`: Review record

  A document of type "review" that records the results of reviewing
  one document.  This includes the issue log, and the brainstormed
  improvement suggestions.

_`.doc.issue`: Issue log

  A record of issues raised during the logging meeting, specifying
  their location, type, finder, and a brief description.  The issue
  log also gives each issue an identifying number.

_`.doc.rev`: Revised document

  The result of performing the edit procedure on the Product document.

_`.doc.acc`: Accepted document

  The result of a Revised document passing exit.

_`.doc.rule`: Rule

  A rule set that a Product document is expected to obey.

_`.doc.guide`: Guidelines

  A "guide" document that a Product document may be expected to be in
  line with.  [Explain how this is distinct from rules.  RB
  2023-01-21]

_`.doc.check`: Checklist

  A list of questions, a negative answer to which indictes that a rule
  has been broken (see .doc.rule).

_`.doc.entry`: Entry criteria

  Criteria that should be met before review to ensure that the
  document is likely to pass exit.

_`.doc.proc`: Procedures

  Descriptions of the steps involved in completing any part of process
  (development, review, or otherwise).

_`.doc.imp`: Brainstormed improvement suggestions

  Suggested improvements to process (and hence to some document)
  arising from the process brainstorm.

_`.doc.request`: Requests for change

  An issue that the editor cannot deal with that is escalated to some
  other tracking system, usually MM Evolution (see process.darwin).


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

.. [MM_guide.review.edit] "Guidelines for review edits"; Gavin
			  Matthews; Harlequin Limited; 1996-10-31;
			  mminfo:guide.review.edit;
			  //info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mminfo/guide/review/edit/index.txt#1.

.. [MM_process.review] "The review process"; Richard Brooksby;
		       Harlequin Limited; 1995-08-18;
		       mminfo:process.review;
		       //info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mminfo/process/review/index.txt#1.

.. [MM_proc.review.brainstorm] "Procedure for process brainstorm in
			       review"; Gavin Matthews; Harelquin
			       Limited; 1997-06-12;
			       mminfo:proc.review.brainstorm;
			       //info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mminfo/proc/review/brainstorm/index.txt#1.

.. [MM_proc.review.check] "Procedure for checking in review"; Gavin
			  Matthews; Harlequin Limited; 1997-06-12;
			  mminfo:proc.review.check;
			  //info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mminfo/proc/review/check/index.txt#1.

.. [MM_proc.review.entry] "Procedure for review entry"; Gavin
			  Matthews; Harlequin Limited; 1997-06-02; mminfo:proc.review.entry;
			  //info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mminfo/proc/review/entry/index.txt#1.

.. [MM_proc.review.exit] "Procedure for exiting a document from
			 review"; Gavin Matthews; Harlequin Limited;
			 1997-06-12; mminfo:proc.review.exit;
			 //info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mminfo/proc/review/exit/index.txt#1.

.. [MM_proc.review.ko] "Procedure for a review kickoff meeting"; Gavin
		       Matthews; Harlequin Limited; 1997-06-12;
		       mminfo:proc.review.ko;
		       //info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mminfo/proc/review/ko/index.txt#1.

.. [MM_proc.review.log] "Procedure for review logging meeting"; Gavin
			Matthews; Harlequin Limited; 1997-06-12;
			mminfo:proc.review.log;
			//info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mminfo/proc/review/log/index.txt#1


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
