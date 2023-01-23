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

This is the procedure for reviewing a change to the MPS in order to
`prevent defects <2. Purpose>`_.

[Insert time-to-execute estimates here based on measurements during
testing of this procedure.  RB 2023-01-23.]

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

[What about getting required changes in to the MPS?  Review is not
purely obstructive.  RB 2023-01-23]

A defect is a way in which the work does not meet its requirements.

A major defect is a defect that

  will probably have significantly increased costs to find and fix
  later in the development process, for example in testing or in use.

  -- guide.review.class.major [Needs importing.  RB 2023-01-23]

As with any procedure, you can vary this one to meet this purpose, but
you should probably read section [Insert reference to Rationale here.
RB 2023-01-20].


3. Review Roles
---------------

_`.role`: People taking part in review are given *roles*.

_`.role.all`: The leader (`.role.leader`_) must ensure that all roles
are assigned to someone, to ensure that everything necessary gets
done.

_`.role.everyone`: Every person taking part in review should be assigned at
least one role, to make it clear what they need to do.

[See also book.gilb93.proc.* to cover what roles need to do during
phases.  RB 2023-01-20]

_`.role.leader`: The *leader* organises and plans the review, and
ensures the procedures are executed.  The leader is responsible for
managing the process in all respects for productive results.  The
author (`.role.author`_) can lead, but this should be avoided if not
an experienced leader.  It is helpful if the leader has received
special instruction in review or inspection.

_`.role.author`: The *author* is the person responsible for the change
under review (`.doc.product`_).  For example, they're the developer
who submitted a pull request.

_`.role.checker`: A *checker* checks the change during review.  There
must be more than one checker.  Every person taking part in a review
is usually also a checker, including the author.  Checkers should be
asked by the leader to check with certain *checking roles* in mind;
this is to increase coverage and reduce duplication of issues.
[Checking roles are available in
mminfo:role.check.{backwards,clarity,consistency,convention,correctness,source}
and these need to be referencable from here, possibly included in this
document. RB 2023-01-21] ["Checking role" is too easily conflated with
"review role" and should perhaps be renamed to method.  RB 2023-01-23]
Checkers also take part defect prevention in `.phase.brainstorm`_.

_`.role.editor`: The *editor* is the person responsible for acting on
the issues found during review in order to bring the work to review
exit.  The editor and `.role.author`_ are usually the same person.
For example, they're the developer who submitted a pull request and
needs to fix it up before it can be merged.

_`.role.qal`: The *quality assurance leader* edits process to prevent
future occurences of defects found by review.  [This comes from
mminfo:book.gilb93.proc.quality.  In the MM Group, developers were
expected to edit process as well.  If possible we'd like the editor to
do this, so that they are involved and committed to process
improvement and defect prevention.  But in the public FOSS MPS we
might not be able to require a submitting developer to do that, so we
should separate the role.  But "quality assurance leader" isn't a
great name; what about "improver" from "process improvement"?  RB
2023-01-23]

_`.role.scribe`: The scribe is the person who records information (not
just issues) during review meetings.  They are usually the same person
as `.role.leader`_.  During `.phase.check`_, review tools will often
allow checkers to record issues as they check, in which case the
scribe should just ensure that this has been done.  But the script
also records information during other phases, such as how much time a
review took, who was there, who did what, etc.  [Make sure necessary
information to record is documented in this procedure.  RB 2023-01-23]

[The following two roles may not be relevant to this section.  They do
not need to be assigned during a review.  RB 2023-01-23]

_`.role.chief`: [Chief Inspection Leader in book.gilb93.  Need to look
this up.  Probably an organizational role to do with communicating
improvements to the review process.  Not relevant to the MPS.  RB
2023-01-20]

_`.role.manager`: The *manager* ensures adequate resources are
assigned to review and that reviews are happening.  [Project Manager
in book.gilb93.  Need to look this up.  Probably not required to
attend the review, and therefore is not assigned. RB 2023-01-20]


4. Phases
---------

_`.phase`: To review a change, the following procedures are executed
more-or-less in the order below.

#. _`.phase.request`: `.role.author`_ requests that their change be
   reviewed.  For example, they submit a GitHub pull request, or
   change a pull request state from "draft" to "ready to review".

#. _`.phase.entry`: `.role.leader`_ executes *review entry*
   (`.entry`_).  If the change doesn't meet the entry criteria then it
   is rejected, and the rest of the review process is not executed.  A
   `.role.author`_ who is an experienced `.role.leader`_ can do entry
   on their own work.

#. _`.phase.planning`: `.role.leader`_ execute `.planning`_ to prepare
   the review and arrange for it to happen.

#. _`.phase.kickoff`: `.role.leader`_, `.role.checker`_ execute
   `.ko`_, beginning the review.

#. _`.phase.check`: `.role.checker`_ execute `.check`_ alone according
   to their checking roles [ref?], looking for unique *major defects*
   that no other checker will bring to the logging meeting.

#. _`.phase.log`: The `.role.leader`_, the `.role.scribe`_, and
   `.role.checker`_ execute `.log`_ to record what has been found, but
   also to stimulate one another to find more *major defects*.

#. _`.phase.brainstorm`: `.role.leader`_, `.role.scribe`_,
   `.role.checker`_, execute `.brainstorm`_ to come up with ways of
   preventing defects in future.

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

#. _`.phase.quality`: The `.role.qal`_ takes action to prevent major
   defects by correcting *process causes*.  Actions include adding
   rules or checklist items, updating procedures, creating tools, or
   adding automated checks.  But they might also include raising
   concerns with management, suggesting wholesale review of working
   practices, requesting training for staff, and so on.

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

_`.entry`: The *review entry procedure* should be executed when a
change is submitted for review (`.phase.entry`_).  The purpose of
entry is to check whether the change is ready for review before
planning a review, committing resources, organizing meetings, etc.

_`.entry.record`: Create record for the procedure.  It needs to be
permanent and referenceable.  On GitHub, you can start a comment on
the pull request.  Record a *permalink* to the procedure you're
following (this one) like::

  Executing [review entry](https://github.com/Ravenbrook/mps/blob/d4ef690a7f2a3d3d6d0ed496eff46e09841b8633/procedure/review.rst#51-review-entry)

_`.entry.change`: Determine and record exactly what the change is, and
ensure it can be identified unambiguously and permanently.  For
example, in Git by branch name and commit hash.  [Note: Git fails at
this because merged branches forget their branch points.  We need some
way to fix that.  RB 2023-01-23] On GitHub, this information is
implicitly recorded by commenting on the pull request in
`.entry.record`_.

_`.entry.criteria`: Determine and record the entry and exit criteria.
Examine the types of documents altered by the change (code, design,
etc.) then look up and record *permalinks* to the criteria for those
types (e.g. entry.code, exit.design) along with entry.universal and
exit.universal, which always apply.  [These are available in
mminfo:rule.entry.* and mminfo:rule.exit.universal, and these need to
be referencable from here, probably in their own documents.  RB
2023-01-21]  [Insert example GitHub comment here.  RB 2023-01-23]

_`.entry.check`: Check that the entry criteria hold.  Record any
transgressions.  Decide whether to reject the change from review by
balancing `2. Purpose`_ and cost.


5.2. Review Planning
....................

_`.planning`: The *review planning procedure* should be executed when
a change has passed `.entry`_.  The purpose of planning is to prepare
the review and arrange for it to happen.

_`.plan.record`: Create a record for the procedure.  On GitHub, you
can start a comment on the pull request.  Record a *permalink* to the
procedure you're following (this one) like::

  Executing [review planning](https://github.com/Ravenbrook/mps/blob/d4ef690a7f2a3d3d6d0ed496eff46e09841b8633/procedure/review.rst#52-review-planning)

_`.plan.time`: Estimate the checking rate and time.  A single review
should not have a checking time of more than one hour.  Record your
estimate.  [Insert example GitHub comment.]

_`.plan.schedule`: Plan when this review may take place and who should
attend.  Check with attendees if appropriate.  Record like::

  @thejayps and @UNAA008 will review 2023-01-23 11:00 for about 2h.

_`.plan.source`: Determine and record the source documents
(`.doc.source`_).  This *must* include the the reason the change is
needed in terms of requirements.  [Entry should've ensured this.  RB
2023-01-23]  On GitHub, this can be the GitHub issue linked from the
pull request.

_`.plan.rule`: Determine and record the rules to apply (`.doc.rule`_).
You can use the entry criteria recorded by `.entry.criteria`_ to
select rule sets [from where?  RB 2023-01-23], but also consider the
list of rule sets [where? RB 2023-01-23].  [We might want e.g. rules
that apply to the critical path.  RB 2023-01-23]

_`.plan.check`: Determine and record the checklists to apply [how and
from where?  RB 2023-01-23].

_`.plan.roles`: Determine and record the checking roles to assign
[how?  RB 2023-01-23].

_`.plan.invite`: Invite the checkers (`.role.checker`_) to the kickoff
meeting (`.ko`_).


5.3. Review Kickoff
...................

[Sourced from [MM_proc.review.ko]_ and needs updating.  RB 2023-01-21]

_`.ko`: `.role.leader`_ holds the *review kickoff* meeting to ensure
that the review begins, and that everyone involved has what they need
to carry out their roles.


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

_`.ko.role`: The leader announces or negotiates any checking roles
they wish to assign, and ensures that checkers understand their
assignments.

_`.ko.improve`: The leader announces any relevant metrics and negotiates objectives.

[How about asking people for suggestions or experiments?  RB 2023-01-23]

_`.ko.log`: The leader announces the time of the logging meeting.  This should 
normally be set at the estimate end of the kickoff meeting, plus the estimated 
checking time, plus a short tea-break.  It should not normally be delayed to 
another day.

_`.ko.remind`: The leader reminds checkers of the purpose of review
(see `2. Purpose`_).

_`.ko.author`: The leader reminds the author that they can withdraw the
document from review at any time.

_`.ko.train.check`: The leader checks that checkers are familiar with their tasks and 
solicits any questions or suggestions.


5.4. Review Checking
....................

[Sourced from [MM_proc.review.check]_ and needs updating.  RB 2023-01-21]

_`.check`: The *checking procedure* should be executed by each
individual `.role.checker`_ alone, carrying out their assigned
checking roles [ref?] without conferring with other checkers.  The
purpose of checking is to find *major defects* not found by other
checkers.

[Note: not all issues are local to a line.  RB 2023-01-21]

[This text was in the phase section and might need to be incorporated here:

   The checking phase has a recommended time or rate, but checkers
   have instructions to deviate from that whenever individual
   availability, role, or situation dictates, in order to increase
   productivity.

   The objective of individual checking is to identify a maximum of
   unique major issues which no other checker will bring to the
   logging meeting.  To do this each checker should have at least one
   special "checking role".

RB 2023-01-23]


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

[This text was in the phase section and may need incorporating here.

   The team concentrates on logging items at a rate of at least one
   per minute.  Items logged include potential defects (issues),
   improvement suggestions, and questions of intent to the author.
   The leader permits little other verbal meeting activity.  Meetings
   last as maximum of two hours at the known optimum rate.  If
   necessary, work must be chunked to avoid tiredness.  Optimum
   checking rate for the meeting is determined by the percentage of
   new issues identified in the logging meeting as well as the
   quantity of the documents.

RB 2023-01-23]


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

[This text was in the phase section and may need incorporating here.

   The followup is done by the `.role.leader`_, and make take place any
   time after the brainstorm meeting.  [What is the followup?  RB
   2023-01-20]

   Immediately after each logging meeting time is used to brainstorm
   the process causes of major defects, and to brainstorm improvements
   to remove these causes.  The meeting shall last no more than half
   an hour.  The objective is to maximize production of useful ideas
   and personal commitment to change within that time.

RB 2023-01-23]


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

[This clashes with the idea of a separate `.role.qal`_.  RB
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
  review.  The work under review.  The changes under review.  The work
  product.  [Much of this procedure has been rephrased in term of
  reviewing a *change*, since this is a *change review procedure* and
  the tools, such as GitHub, focus on reviewing change.  Introducing a
  new product document is a change.  RB 2023-01-23]

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
