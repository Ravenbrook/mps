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

.. TODO: Consistent terminology for the work under review, rather than
   "change", "work", "product document", etc.

.. TODO: Check against book.gilb93.proc.* and consider dividing
   procedures by role.

.. TODO: Incorporate MM Group checklists from
   <https://info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mminfo/check/>.

.. TODO: More explicit management of checking rates.

.. TODO: Update "familiar with" to "know".

.. TODO: Explicitly incorporate `irreducible errors
   <https://en.wikipedia.org/wiki/The_Mythical_Man-Month#The_tendency_towards_irreducible_number_of_errors>`__.

.. TODO: More specific links to rationale, [Gilb-93]_ etc. for
   justification and variation.

.. TODO: Lift review record advice, specifically GitHub comment
   format, into a section.  rule.generic.once!

.. TODO: Explicitly say to record how long each step took.


1. Introduction
===============

This is the procedure for reviewing a change to the MPS in order to
prevent defects (`.purpose`_).

This procedure may seem overwhelming at first, but it can be executed
quickly [how quickly? kpa.qpm! RB 2023-01-21] once you have learned it
[and how long does that take?  RB 2023-01-21].  [Insert example times
here for different kinds of work.  RB 2023-01-21]

Time to execute:

- first time, with experienced leader training checkers, on a small
  low-risk change: 3h [cite
  https://github.com/Ravenbrook/mps/pull/117#issuecomment-1405388814
  et seq. RB 2023-01-30]

- express reviews (`.express`_): <30m

[Insert further estimates from more testing.  RB 2023-01-30]

This procedure requires *training*, preferably by an experienced
review leader (`.role.leader`_).  At the very least, do not apply this
procedure to risky changes without first:

- reading and understanding the whole document and the related rules

- practising it on low-risk changes

This is a placeholder while we arrange to bring in documents from
outside the MPS Git repository and tree.  For background, see `GitHub
issue #95 <https://github.com/Ravenbrook/mps/issues/95>`_.

Since this document was created as part of `a project to migrate from
Perforce to Git (and GitHub)
<https://github.com/orgs/Ravenbrook/projects/1>`_, this procedure will
give specifics on conducting review via GitHub.  But the process is in
no way specific to GitHub, Git, Perforce, or any other tool.

This procedure was created by incorporating and updating review
process documents from Ravenbrook and the Harlequin MM Group (see
`A. References`_).  Those documents contain process improvements from
hundreds of reviews and thousands of hours of effort.

The process is largely derived from "Software Inspection" [Gilb-93]_,
which was itself developed from `Fagan inspection
<https://en.wikipedia.org/wiki/Fagan_inspection>`__.  It was developed
over years by Gavin Matthews of the Harlequin Memory Management Group,
a trained inspection leader.

The process is an example of "Peer Review" (kpa.pr), a key process
area of level 3 of the Capability Maturity Model [CMU-SEI-93-TR-025]_,
but also contributes a great deal to:

- Defect Prevention (kpa.dp, level 5)
- Process Change Management (kpa.pcm, level 5)
- Quantitive Process Management (kpa.qpm, level 4)

[Notes for inclusion:
  - Review is not just (or even mostly) looking at diffs, though
    GitHub encourages this idea.
  - Review is not just for code.
  - Check against book.gilb93.handbook [Gilb-93]_.
  - Check against kpa.pr, [CMU-SEI-93-TR-025]_, p L3-97
  - Review is about humans and mind tricks.]


2. Purpose
==========

_`.purpose`: The purpose of the review procedure is:

1. _`.goal.fix`: find and correct `major defects`_

2. _`.goal.prevent`: prevent future defects

[What about getting required changes in to the MPS?  Review is not
purely obstructive.  Perhaps `.goal.fix`_ should be split into "find"
and "fix".  RB 2023-01-23]

_`.def.defect`: A defect is a way in which the work does not meet its
requirements.

_`.def.defect.major`: A major defect is a defect that

  will probably have significantly increased costs to find and fix
  later in the development process, for example in testing or in use.

  -- `.class.major`_

As with any procedure, you can vary this one to meet this purpose, but
you should probably read section `13. Rationale`_.


3. Review Roles
===============

_`.role`: People taking part in review are given *roles*.

_`.role.two`: All the roles can be covered by just two people.

_`.role.all`: The leader (`.role.leader`_) must ensure that all roles
are assigned to someone, to ensure that everything necessary gets
done.

_`.role.everyone`: Every person taking part in review should be assigned at
least one role, to make it clear what they need to do.

_`.role.leader`: The *leader* organises and plans the review, and
ensures the procedures are executed.  The leader is responsible for
managing the process in all respects for productive results.  The
author (`.role.author`_) can lead, but this should be avoided if not
an experienced leader.  It is helpful if the leader has received
special instruction in review or inspection.

_`.role.author`: The *author* is the person responsible for the change
under review (`.doc.product`_).  For example, they're the developer
who submitted a pull request.  The author should read
`.advice.author`_.

_`.role.checker`: A *checker* checks the change (`.doc.product`_)
during review.  There must be more than one checker.  Every person
taking part in a review is usually also a checker, including the
author.  Checkers should be asked by the leader to check with certain
*checking roles* (`.role.check`_) in mind; this is to increase
coverage and reduce duplication of issues.  Not every checker needs to
understand everything about a change (e.g. the programming language)
thoroughly.  Many kinds of checking are useful.  Checkers also take
part defect prevention in `.phase.brainstorm`_.

_`.role.editor`: The *editor* is the person responsible for acting on
the issues found during review in order to bring the work to review
exit.  The editor and `.role.author`_ are usually the same person.
For example, they're the developer who submitted a pull request and
needs to fix it up before it can be merged.

_`.role.improver`: The *process improver* takes action to prevent
future occurences of defects found by review.  This should be the same
person as `.role.editor`_ so that they maintain a good understanding
of (and commitment to) process improvement and defect prevention, but
this isn't always possible, e.g. when `.role.author`_ is an outside
contributor.

_`.role.scribe`: The scribe is the person who records information (not
just issues) during review meetings.  They are usually the same person
as `.role.leader`_.  During `.phase.check`_, review tools (such as
GitHub) will often allow checkers to record issues as they check, in
which case the scribe should just ensure that this has been done.  The
scribe also records information during other phases, such as how much
time a review took, who was there, who did what, etc.  [Make sure
necessary information to record is documented in this procedure under
a uniform tag.  RB 2023-01-23]

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
=========

_`.phase`: This section describes the phases of a review.  Each phase
has a procedure.  The phases involve varying groups of people
(`.role`_) and have diverse purposes.

_`.phase.handbook`: This section can be used as a short "handbook" for
people who have learned the procedure.  (Compare with "A one-page
inspection handbook" [Gilb-93]_.)

_`.phase.order`: To review a change, the following procedures are
executed roughly in the order below.

#. _`.phase.request`: `.role.author`_ requests that their change be
   reviewed.  For example, they submit a GitHub pull request, or
   update the pull request state from "draft" to "ready to review".

#. _`.phase.entry`: `.role.leader`_ executes `.entry`_.  If the change
   doesn't meet the entry criteria then the change fails review, and
   the rest of the review process is not executed.  A `.role.author`_
   who is an experienced `.role.leader`_ can do entry on their own
   work.

#. _`.phase.plan`: `.role.leader`_ executes `.plan`_ to prepare the
   review and arrange for it to happen.

#. _`.phase.kickoff`: `.role.leader`_ and `.role.checker`_ execute
   `.ko`_, beginning the review.

#. _`.phase.check`: `.role.checker`_ individually execute `.check`_,
   according to their checking roles (`.role.check`_), looking for
   unique `major defects`_ that no other checker will bring to the
   logging meeting.  Checking continues during the next phase,
   `.phase.log`_.

#. _`.phase.log`: `.role.leader`_, `.role.scribe`_, and
   `.role.checker`_ together execute `.log`_ to share and record what
   has been found, and to find more `major defects`_, stimulated by
   what has been found so far.  `.phase.check`_ continues during this
   phase.

#. _`.phase.brainstorm`: `.role.leader`_, `.role.scribe`_, and
   `.role.checker`_, execute `.brainstorm`_ to come up with ways of
   preventing defects in future.

#. _`.phase.estimation`: `.role.leader`_, `.role.scribe`_, and
   `.role.checker`_ spend a few minutes using `.calc`_ to estimate how
   productive the review was, by:

   - estimating the cost of the review (mostly work hours)
   - projecting what the defects would cost if uncorrected
   - projecting what similar defects would cost if not prevented

   and `.role.scribe`_ records this information.

#. _`.phase.edit`: `.role.editor`_ executes `.edit`_, analysing and
   correcting defects, but taking *some* action on *every* issue.
   This produces the *revised change* (`.doc.rev`_).

#. _`.phase.pi`: `.role.improver`_ executes `.pi`_ to prevent `major
   defects`_ by correcting *causes*.

#. _`.phase.exit`: `.role.leader`_ executes `.exit`_.  If the revised
   change does not meet the exit criteria then it fails review.
   Otherwise it passes and can go on to be used, e.g. by being merged
   into the master codeline (`proc.merge.pull-request`_).

Even the express review procedure (`.express`_) has these phases.

.. _proc.merge.pull-request: pull-request-merge.rst

.. _major defects: `.def.defect.major`_


5. Procedures
=============

5.1. Review Entry
-----------------

_`.entry`: The *review entry procedure* should be executed when a
change is submitted for review (`.phase.entry`_).

_`.entry.purpose`: The purpose of entry is to check whether the change
is ready for review before planning a review, committing resources,
organizing meetings, etc.

_`.entry.express`: Does this change look low risk?  Is someone
available?  Consider the *express review procedure* (`.express`_).

_`.entry.record`: Record the entry procedure (`.doc.record`_).

- On GitHub, you can start a comment on the pull request.

- Record a the procedure you're following (this one).  Use a
  permalink.  For example::

    Executing [proc.review.entry](https://github.com/Ravenbrook/mps/blob/d4ef690a7f2a3d3d6d0ed496eff46e09841b8633/procedure/review.rst#51-review-entry)

    1. Start time 11:03.

_`.entry.change`: Record exactly what the change is.

- On GitHub, this information is implicitly recorded by commenting on
  the pull request in `.entry.record`_.

- Otherwise, record something like the branch name and commit hash.
  [Note: Git fails at this because merged branches forget their branch
  points.  We need some way to fix that.  RB 2023-01-23]

_`.entry.criteria`: Determine and record the entry and exit criteria.

- `entry.universal`_ and `exit.universal`_ always apply.

- Add criteria for the types of documents altered by the change (code,
  design, etc.) from the `procedure directory`_.

- Record permalinks to the criteria.  For example::

    Executing [proc.review.entry](https://github.com/Ravenbrook/mps/blob/d4ef690a7f2a3d3d6d0ed496eff46e09841b8633/procedure/review.rst#51-review-entry)

    1. Start time 11:03.

    2. Applying [entry.universal](https://github.com/Ravenbrook/mps/blob/eceaccdf5ab8d8614e9a8bb91a23bdcb99e7d0ce/procedure/entry.universal.rst) and [entry.impl](https://github.com/Ravenbrook/mps/blob/eceaccdf5ab8d8614e9a8bb91a23bdcb99e7d0ce/procedure/entry.impl.rst).  

_`.entry.check`: Check that the entry criteria hold.  Record any
transgressions.  Decide whether to reject the change from review by
balancing `.purpose`_ and cost.  Will it pass `.exit`_?

_`.entry.metrics`: Record the time taken to execute `.entry`_.

.. _entry.universal: entry.universal.rst

.. _exit.universal: exit.universal.rst

.. _procedure directory: ./


5.2. Review Planning
--------------------

_`.plan`: The *review planning procedure* should be executed when
a change has passed `.entry`_.

_`.plan.purpose:` The purpose of planning is to prepare the review so
that it is efficient and effective, and arrange for it to happen.

_`.plan.record`: Record the planning procedure.

- On GitHub, you can start a comment on the pull request.

- Record the procedure you're following (this one).  Use a permalink.
  For example::

    Executing [proc.review.plan](https://github.com/Ravenbrook/mps/blob/d4ef690a7f2a3d3d6d0ed496eff46e09841b8633/procedure/review.rst#52-review-planning)

    1. Start time 11:31.

_`.plan.iterate`: Consider all of this procedure.

- This procedure is only in rough order.  Later steps may change
  earier decisions.  For example, availability of people for
  `.plan.roles`_ might affect `.plan.tactics`_.

_`.plan.tactics`: Examine the change and decide how to check it to
achieve the `.purpose`_.

- The default and most effective tactic is to have `.role.checker`_
  examine every line of the change, evenly distributing their
  attention by using a checking rate, such as 10 lines/minute.

- Large repetitive automatic changes (seach-and-replace) might be best
  handled by sampling using a random number generator and a strong
  Brownian motion producer (dice and tea).

- Large changes might be broken up by document type, or topic, but you
  still want multiple `.role.checker`_ to look at everything.

- Changes that cannot feasibly be checked
  (`entry.universal.reviewable`_) may need to be reworked into stages,
  perhaps by version control transformations.
  [branch/2014-02-19/remember-time ->
  branch/2014-04-14/remember-time-2 ->
  branch/2016-03-22/remember-time-3 -> branch/2018-08-08/refset-struct
  is an example of this.  RB 2023-01-31]

- Record any variations from the default tactic.

.. _entry.universal.reviewable: entry.universal#reviewable

_`.plan.time`: Estimate the checking rate and time.

- GitHub provides diff stats on the pull request (to the right of
  "Conversation").

- `.phase.check`_ should last no more than one hour, so that checkers
  can maintain concentration.

- `.phase.log`_ should last no more than two hours, so that checkers
  can maintain concentration.

- It may be necessary to divide the review into multiple sessions.

- Record your estimates.  For example::

    Executing [proc.review.plan](https://github.com/Ravenbrook/mps/blob/d4ef690a7f2a3d3d6d0ed496eff46e09841b8633/procedure/review.rst#52-review-planning)

    1. Start time 11:31.

    2. proc.review.plan.time: About 500 lines of code @ 10 lines/minute
       so about 50 mins of checking. 

_`.plan.schedule`: Plan when this review may take place and who should
attend.  Negotiate with attendees if appropriate.

- Record like::

    3. proc.review.plan.schedule: @thejayps and @UNAA008 will review
       2023-01-23 11:00 for about 2h.

_`.plan.train`: Ensure that all participants are familiar with the
review process.

- Brief anyone new to the process about how it works and what is
  expected of them.

- Ensure that they have the process documents.

- Allow extra time for training.

_`.plan.source`: Determine and record the source documents that could
be used for checking (`.doc.source`_).

- Always include issues resolved or partially resolved by the change.
  There must be at least one (ensured by `.entry.criteria`_).

- Consider requirements, issues, designs, analysis, discussions,
  records of failures (e.g. in email messages), user documentation,
  standards.

_`.plan.rule`: Determine and record the rules to apply (`.doc.rule`_).

- Add rules for the types of documents altered by the change (code,
  design, etc.) from the `procedure directory`_.

- Also select other rules that apply from the `procedure directory`_,
  for example special rules that apply to the critical path.  [Needs
  example.  RB 2023-01-28]

_`.plan.check`: Determine and record the checklists to apply [how and
from where?  See `mminfo:check.* <https://info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mminfo/check/>`__.  RB 2023-01-23].

_`.plan.roles`: Decide and record the checking roles (`.role.check`_)
to assign.

- Consider and try to assign every checking role (`.role.check`_).

- Choose checking roles that are most likely to find `major defects`_
  in the type of change under review.

- Always try to assign `.role.check.backwards`_ or a similar
  out-of-order sampling method, to help find defects in all parts of
  the change.

- Bear in mind that `.role.leader`_ and `.role.scribe`_ will be
  somewhat occupied during logging and less able to check.

- Assignments can be renegotiated in `.ko.role`_.

_`.plan.homework`: Assign work that people should do before the
review.

- Include background reading or other self-education that will help
  review efficiency.  For example, reading about a technical aspect of
  the change.

- You should not request review activities like studying source
  documents or looking at the change.  Plan properly.

- Plan the review to function successfully (but perhaps take longer)
  even if the work is not done.

_`.plan.invite`: Invite the checkers (`.role.checker`_) to the kickoff
meeting (`.ko`_).

_`.plan.doc`: Ensure that `.role.checker`_ have all the documents they
need (the change, source documents, rules, etc.)

_`.plan.metrics`: Record the time taken to execute `.plan`_.


5.3. Review Kickoff
-------------------

_`.ko`: `.role.leader`_ holds the *review kickoff* meeting to ensure
that the review begins, and that everyone involved has what they need
to perform their roles.

_`.ko.record`: Record the kickoff procedure.

- On GitHub, you can start a comment on the pull request.

- Record the procedure you're following (this one).  Use a permalink.
  For example::

    Executing [proc.review.kickoff](https://github.com/Ravenbrook/mps/blob/b2050e2cf69029fc13c31a724421945952d3fab2/procedure/review.rst#53-review-kickoff)

    1. Start time 15:00.

_`.ko.doc`: Ensure that every checker has all the documents they need.

_`.ko.intro`: Optionally, ask the author for a short (one minute)
introduction to the change.

- Listen for new information this reveals and start the `.log.record`_
  early if there's anything that needs documenting, such as a hidden
  assumption or requirement.  This happens!

_`.ko.remind`: The leader reminds everyone of the `.purpose`_ of
review.

- Remind `.role.checker`_ that they are trying to find unique `major
  defects`_ not found by other checkers.

- Remind `.role.checker`_ to avoid conferring until `.log`_.

- Ask `.role.checker`_ to avoid finishing GitHub reviews or submitting
  "single comments" until `.log`_.

_`.ko.role`: Negotiate checking roles (`.role.check`_).

- `.role.checker`_ can volunteer for roles based on how they feel at
  the time.  Focus and enjoyment are important for good results.

- Ensure checkers understand their checking roles and checking rates
  [ref?  RB 2023-02-02].

- Record who's doing what.

_`.ko.train`: Offer private help to new `.role.checker`_ after `.ko`_
so that you don't delay `.check`_.

_`.ko.improve`: Announce any review metrics and negotiate review
objectives.

- Announce the rate 

- Ask for suggestions or experiments with review procedure.

- Record metrics and objectives.

_`.ko.log`: Set a time for the logging meeting (`.log`_).

- This should normally be set at the estimated end of `.ko`_, plus the
  estimated checking time (see `.plan.time`_), plus a short break.
  Avoid delay.

_`.ko.author`: Remind the author that they can withdraw the document
from review at any time.

_`.ko.go`: Send `.role.checker`_ away to start `.check`_.

_`.ko.metrics`: Record the time taken to execute `.ko`_.


5.4. Review Checking
--------------------

_`.check`: The *checking procedure* should be executed by each
individual `.role.checker`_ alone, carrying out their assigned
checking roles (`.role.check`_) without conferring with other
checkers.

_`.check.purpose`: The purpose of checking is to find unique `major
defects`_ that no other checker will bring to `.log`_.


5.4.1. Start
............

_`.check.doc`: Ensure that you have all the documents you need to
perform your checking role (`.role.check`_).

_`.check.ask`: Ask `.role.leader`_ if you have any questions about
checking.


5.4.2. Checking
...............

_`.check.record`: You can note what you find in any way you like.

_`.check.record.start`: Make a note of the start time.

_`.check.record.github`: You can note issues using GitHub's review
tool in a way that will save time during `.log`_.

#. Open the "Files changed" tab of the pull request.

#. Hover over the line where you want to make a note.

#. Click the green "+" button.

#. Type your note, e.g. "M: overruns array bounds".  It will help if
   you use `.log.format`_.

#. Then press "Start a review".

#. Repeat for other notes.

#. Do not "finish" your review before `.log`_ to avoid distracting
   other `.role.checker`_.

_`.check.diff.not`: Do not check using diffs unless your checking role
says so.  Check the work *as it will be after the change* only using
the diffs to help direct attention.

_`.check.source`: Read `.doc.source`_ for your `.role.check`_.

- Don't spend time searching for defects in `.doc.source`_.  If you
  happen to find any, that's a bonus.  Note them for logging as
  `.class.imp`_ and possibly `.class.major`_ as well.

_`.check.rule`: Ensure that you know `.doc.rule`_ and `.doc.check`_.

- If they've changed since you last read them, study and understand
  the changes.

_`.check.role`: Ensure that you know `.role.check`_ and keep it in
mind as you check.

_`.check.product`: Check `.doc.product`_.

_`.check.rate`: Try to check at the planned checking rate
(`.plan.time`_).  Do not rush.  Slower is usually better.  Control
your attention.

_`.check.major`: Concentrate on finding `major defects`_.

_`.check.max`: Find as many issues as possible to help the author.

_`.check.note`: Note all issues; you need not log them later.

_`.check.rough`: Your notes can be rough.  `.check.major`_!

- Do not spend time making your issues neat and clear or even putting
  them in exactly the right place.  Save that for `.log`_.  Search for
  more issues.  `.check.major`_!

_`.check.trouble`: Consult `.role.leader`_ if you're having trouble:

- you have questions
- you are finding too many or too few issues

_`.check.class`: Classify each issue you find (`.class`_).


5.4.3. End
..........

_`.check.metrics`: At the end of checking, record

- how many issues you found, by class (see `.check.class`_)

- how long you actually spent checking

- how much of the product document you actually checked

- any problems encountered

_`.check.metrics.github`: You can record your metrics in a GitHub
review.

#. Open the "Files changed" tab of the pull request.

#. Click the green "Review changes" button.

#. Enter metrics in the text box.

#. Do not "finish" your review before `.log`_ to avoid distracting
   other `.role.checker`_.


5.5. Review Logging
-------------------

_`.log`: The *review logging procedure* executed by `.role.leader`_
and `.role.scribe`_ together with `.role.checker`_.

_`.log.purpose`: It has two purposes:

1. to record issues for action

2. to find more `major defects`_, stimulated by sharing what has been
   found so far

_`.log.check`: Checking continues during logging.

_`.log.advice`: Remind the author of `.advice.author`_.

_`.log.author`: Remind the author that they can withdraw
`.doc.product`_ from review at any time.

_`.log.record`: `.role.scribe_` should record the logging procedure.

- On GitHub, you can start a comment on the pull request.

- Record the procedure you're following (this one).  Use a permalink.
  For example::

    Executing [proc.review.log](https://github.com/Ravenbrook/mps/blob/12160d613b04045d6bd5380932f7560c91647556/procedure/review.rst#55-review-logging)

    1. Start time: 15:50.

- This opens `.doc.log`_.  `.role.scribe`_ can append issues to the
  log, but see `.log.record.github`_.

_`.log.record.github`: Ask `.role.checker`_ using the GitHub review
tool to "finish" their reviews now, so that their notes and metrics
are automatically included in `.doc.log`_.  `Major defects`_ recorded
in this way must still be "logged" by announcing them to the meeting
(`.log.major`_).

_`.log.sums`: "How many did you find?"  Gather, sum, and record
individual metrics from `.check.record`_ of:

- how many issues were found, by class (see `.check.class`_)

- how long was spent checking

- how much of the product document was checked

_`.log.decide`: Now, and at intervals during logging, assess whether
`.doc.product`_ is likely to pass `.exit`_.  If it seems very
unlikely, consult with `.role.author`_ and `.role.editor`_ about
aborting the logging meeting.  Bear in mind:

- Second reviews often find fewer issues, so it may be worth logging
  them anyway.

- `.brainstorm`_ needs `major defects`_ to work on, and might prevent
  whatever went wrong here.

- The MM Group never aborted logging.

_`.log.plan`: Use the metrics to decide a logging rate.

- The rate should be at least one per minute.  [Find this advice in
  [Gilb-93]_.  RB 2023-01-29]

- Try to get all issues are logged during scheduled meeting time.

- Slow down if many new issues are being found.  Speed up if not.
  `.role.checker`_ should tell you when they find issues
  (`.log.new`_).

- Schedule breaks to maintain concentration.

- Consider scheduling more logging meetings.

_`.log.scribe`: Assign `.role.scribe`_ (usually the leader), and
ensure `.role.editor`_ will find and be able to read the log.

_`.log.explain`: `.role.leader`_ ensures `.role.checker`_ understand
the order in which issues will be logged.

_`.log.format`: `.role.leader`_ ensures `.role.checker`_ understand
the desired form of issues, namely:

- location

- `.class`_, including `.class.new`_ if the issue was discovered
  during logging

- how it breaks which `.doc.rule`_ or `.doc.check`_, if known,
  otherwise briefly what's wrong ("typo", "uninitialized", "obi-wan",
  "missing requirement", etc.)

_`.log.dup`: `.role.leader`_ can remind `.role.checker`_ to avoid
logging issues that have are duplicates of ones already logged.

_`.log.order`: Ask `.role.checker`_ to try to list their issues in
forwards document order.  This makes life easier for other checkers
and the editor.  (There has been much experimentation with the order
of logging, but was most effective the MM Group.)

_`.log.major`: `.role.leader`_ calls upon `.role.checker`_ in turn to
announce `major defects`_ they found.

- `.role.scribe`_ ensures that `major defects`_ are recorded so that
  they are *all* actioned by `.edit`_ and `.pi`_.

- On GitHub, the scribe can start a new GitHub review to record issues
  not already recorded, as in `.check.record.github`_, or make "single
  comments" from the diffs, or just enter them in comments on the pull
  requests.

_`.log.fast`: Log issues briskly.  Allow people to clarify the issue,
but discourage discussion.  Encourage the search for more `major
defects`_.  `.role.leader`_ should firmly discourage discussion of:

- whether issues are genuine defects

- how a defect may be resolved

- the review process (other than to answer questions);

- the answers to questions logged

_`.log.slow`: Log issues slowly enough that `.role.checker`_ have time
to understand issues and use them to find more `major defects`_.

_`.log.new`: When `.role.checker`_ find new `major defects`_ they
should:

- tell `.role.leader`_, for counting

- note them as they did during `.check`_ and announce them later in
  `.log.major`_ but ensure they are classified as "new"
  (`.class.new`_).

_`.log.decide.non-major`: After logging `major defects`_, decide
whether and how many minor issues (`.class.minor`_) to log during the
meeting, considering `.log.purpose`_.

- Avoid fatigue.

- `.role.checker`_ may have already noted minor issues in a way that
  can be found during `.edit`_, such as in GitHub comments.

- Perhaps ask `.role.checker`_ to cherry-pick a fraction of their
  minor issues and submit the rest later.  

- `.role.checker`_ should cherry-pick issues that have the best chance
  of helping to find `major defects`_ or prevent them via
  `.brainstorm`_.

_`.log.non-major`: Go through `.doc.product`_ in sections (or
equivalent), at each stage announce the section, ask who has issues,
and request the issues.

- `.role.scribe`_ ensures the issues are recorded (see `.log.major`_).

- This is a good time to log `.class.imp`_ (issues outside
  `.doc.product`_) that came up while reviewing specific parts of
  `.doc.product`_.

_`.log.general`: Ask `.role.checker`_ in turn for any general or new
issues not already logged.

- `.role.scribe`_ ensures the issues are recorded (see `.log.major`_).

_`.log.brainstorm`: Negotiate a time for the `.brainstorm`_.  This
will normally be after a break at the end of `.log`_.

_`.log.inform`: Inform `.role.editor`_ that `.doc.product`_ is ready for
`.edit`_.

_`.log.metrics`: Record the time taken to execute `.log`_.


5.6. Review Brainstorm
----------------------

_`.brainstorm`: The *review brainstorm procedure* should be executed
by `.role.leader`_ with `.role.scribe`_ and `.role.checker`_ very soon
after `.log`_.

_`.brainstorm.purpose`: The purpose of review brainstorm is to come up
with ways of preventing defects in future (`.goal.prevent`_).

_`.brainstorm.time`: The process brainstorm should last no more than
around 30 minutes.

_`.brainstorm.record`: Record the brainstorm procedure
(`.doc.record`_).

- On GitHub, you can start a comment on the pull request.

- Record a the procedure you're following (this one).  Use a
  permalink.  For example::

    Executing [proc.review.brainstorm](https://github.com/Ravenbrook/mps/blob/branch/2023-01-19/review-procedure/procedure/review.rst#56-review-brainstorm)

    1. Start time: 16:33.

_`.brainstorm.choose`: Choose 3 to 6 `major defects`_ or groups
of `major defects`_ found in review.

- Make this choice based on defect importance and your experience of
  which defects can be most profitably attacked.

- Record the issues, e.g. by pasting links into the record comment, so
  you can edit them for `.brainstorm.log`_.
  
_`.brainstorm.remind`: Remind everyone of `.brainstorm.purpose`_ and
`.pi.scope`_.

_`.brainstorm.focus`: Ask everyone *not* to spend time analysing the
defects found by the review, or suggesting ways to fix those defects,
except insofar as it is necessary to develop ways to *prevent* those
defects.

_`.brainstorm.raise`: Raises each major defect in turn, reminding
participants of the issue, and asking how it happenned and what could
have prevented it.

_`.brainstorm.disc`: Encourage discussion for no more than about five
minutes per defect.  Focus on how the defect arose, and what
improvement could prevent it.  Curtailing discussion of how the defect
can be fixed.

_`.brainstorm.proc`: If time permits, the leader may solicit
criticisms of the review process and apply `.brainstorm.disc`_ to
them.

_`.brainstorm.log`: Record the suggestions like::

  2. For https://github.com/Ravenbrook/mps/pull/117#discussion_r1090530823
     : @thejayps suggests a checklist item perhaps, where you
     deliberately try to misinterpret your sentences and improve
     them if you can (misinterpret them).

_`.brainstorm.metrics`: Record the time taken to execute `.brainstorm`_.


5.7. Review Edit
----------------

_`.edit`: The *review edit procedure* must be executed by
`.role.editor`_ to revise `.doc.product`_ into `.doc.rev`_ by
processing `.doc.log`_.

_`.edit.purpose`: The purpose of the review edit is to analyse and
correct defects, part of the review's primary purpose (`.goal.fix`_).

_`.edit.record`: Record the edit procedure.

- On GitHub, you can start a comment on the pull request.

- Record the procedure you're following (this one).  Use a permalink.
  For example::

    Executing [proc.review.edit](https://github.com/Ravenbrook/mps/blob/f8b6c94be9304d017d8a5cf57f7f4ab367ac51fc/procedure/review.rst#57-review-edit)

    1. Start time 2023-02-02 09:45.

_`.edit.record.time`: Edit might take several sessions.  Keep track of
the working time spent for `.edit.metrics`_.

_`.edit.read`: Locate and read all of `.doc.log`_ before making any
edits.

- On GitHub, the log should be visible as comments and reviews on the
  pull request, starting at the kickoff record (`.ko.record`_).

_`.edit.log`: Record your actions in one of these ways (in order of
preference):

- Respond to the issue like a conversation.  This works well for
  GitHub review comments.

- Quote the text of the issue in a comment.  This works well for
  issues in comments on a pull request.

- Edit the log and record your action in a comment, e.g. ::

    m: Warthog too warty.  [Fixed: Warts reduced in f93b75dc]

- Append your action to the `.edit.record`_ with a reference.

- In any case, your actions must be recorded permanently in a way that
  is traceable from `.doc.log`_.

_`.edit.act`: You must take action on every issue in `.doc.log`_ and
record that action.  Record one of the following responses:

_`.edit.act.fix`: Fix the defect and say a few words about how.
Always say where.

- Write "Fix: <how> in <commit>"

_`.edit.act.reject`: Reject the issue with a reason why it is not a
valid issue.

- Write "Reject: <reason>"

_`.edit.act.comment`: Add a comment to `.doc.product`_ rather than
"fixing" the issue.  Say why the issue cannot be fixed.  Note that
this is not the same as fixing a defect in a comment.

- Write "Comment: <reason> in <commit>"

_`.edit.act.raise`: Escalate for later action, usually by creating an
issue to go into the project queue, such as a GitHub issue.

- Write "Raise: <reference>"

- This can apply to `.class.question`_ if it a difficult one.

_`.edit.act.forget`: Decide that the issue is not worth an action,
even though it's valid.  Give your reason.

- Write "Forget: <reason>"

- Use with caution, and *never* for `.class.major`_.

_`.edit.act.answer`: For `.class.question`_, give an answer, and tag
or message the questioner so that they see it.

- Write "Answer: <answer>"

- You can send an answer by some other traceable means and link it.

_`.edit.act.imp`: Pass the issue to another person, and ensure they
accept it.

- Write "Pass: <person>"

- Mainly intended for `.class.imp`_, where some outside document needs
  an edit.

_`.edit.extra`: You may make corrections to defects which you spot
yourself during editing work.  Log them like those found during
`.check`_ or `.log`_ and inform `.role.leader`_ about them.

_`.edit.exit`: After action has been taken and recorded on every
logged issue, tell `.role.leader`_ that the revised change is ready
for `.exit`_.

_`.edit.metrics`: Record both the working time spent and the end time
of `.edit`_.


5.8. Process Improvement
------------------------

_`.pi`: The *process improvement procedure* must be executed by
`.role.improver`_ to take action to prevent future defects by
processing `.doc.log`_, but especially the results of
`.brainstorm`_.

_`.pi.purpose`: The purpose of process improvement is to take action
to prevent future defects, closing the process improvement loop
(`.goal.prevent`_).

_`.pi.scope`: The scope of actions that might be taken by the improver
should not be limited, and could include:

- filing process issues for later action
- raising concerns with management
- sending suggestions to anyone
- suggesting wholesale review of working practices
- requesting training for staff.

as well as changes like:

- adding rules (`.doc.rule`_) or checklist items (`.doc.check`_)
- updating procedures (`.doc.proc`_)
- updating or writing guides (`.doc.guide`_)
- creating tools
- adding automated checks

_`.pi.record`: Record the process improvement procedure.

- On GitHub, you can start a comment on the pull request.

- Record the procedure you're following (this one).  Use a permalink.
  For example::

    Executing [proc.review.pi](https://github.com/Ravenbrook/mps/blob/f8b6c94be9304d017d8a5cf57f7f4ab367ac51fc/procedure/review.rst#58-process-improvement)

    1. Start time 2023-02-02 11:45.

_`.pi.record.time`: See `.edit.record.time`_.

_`.pi.read`: Locate and read all of the suggestions recorded in
`.brainstorm.log`_ before making any decisions.

_`.pi.log`: Record your actions in the same manner as edit actions
(`.edit.log`_).

_`.pi.act`: You must take a written action for every improvement
suggestion and record that action.  Record your response like an edit
(`.edit.act`_).

_`.pi.exit`: After action has been taken and recorded on every
suggestion, tell `.role.leader`_.  [This procedure doesn't make it
clear how the leader tracks and receives this information, when it
times out, etc.  RB 2023-02-01.]

_`.pi.metrics`: See `.edit.metrics`_.


5.8. Review Exit
----------------

_`.exit`: The *review exit procedure* is should be executed by
`.role.leader`_ after editing (`.edit`_).

_`.exit.purpose`: The purpose of exit is to determine whether the
revised change passes review.

_`.exit.record`: Record the exit procedure (`.doc.record`_).

- On GitHub, you can start a comment on the pull request.

- Record a the procedure you're following (this one).  Use a
  permalink.  For example::

    Executing (proc.review.exit)[https://github.com/Ravenbrook/mps/blob/645200a25e5e415a2a2978d550b5251e0284c43e/procedure/review.rst#58-review-exit]

    1. Start time 10:20.

_`.exit.check`: Check that the exit criteria hold (see
`.entry.criteria`_).

- Record any transgressions, like::

    2. exit.universal.quest: Question 5 answered in chat but not in docs.
 
_`.exit.fix`: Fix transgressions, if it is feasible with low risk.
Otherwise ask `.role.editor`_ to fix them.  Record this action, and
record edits in the same way as `.edit`_.

_`.exit.fail`: If transgressions remain, then the revised change is
too defective.  It fails review and must not be used.

- Record this result, like::

    3. Revised change rejected.

- Tell someone.  [Who and how?  RB 2023-01-28]

_`.exit.pass`: Otherwise, the revised change passes review and can be
used.

- Record this result, like::

    3. Revised change passed.

- On GitHub, the approve the pull request for merge.

- Tell the person who will put the change to use, such as someone who
  will merge it to master.

_`.exit.calc`: Calculate and record final review metrics (`.calc`_).
For example::

    4. review.exit.calc:
       - hours used: 11
       - hours saved: 70
       - major defects remaining: 1.5

_`.exit.inform`: Inform all review participants of the result of their
efforts.

_`.exit.metrics`: Record the time taken to execute `.exit`_.


6. Documents
============

[Sourced from [MM-process.review]_ and needs updating.  RB 2023-01-21]

_`.doc`: The review process involves a lot of documents.  This is a
brief explanation of what they are.

_`.doc.forms`: Documents come in many forms.  They might be web pages,
email messages, GitHub comments, chat messages, and sometimes even
printed on dead trees.

_`.doc.source`: Source document
  A document from which the product document is derived.  Note that
  this does not mean "source code".

  For example, a failure of the software might result in a *failure
  report*, which gets logged to an *issue*, where someone writes an
  *analysis* and *designs* a solution.  All of those things are source
  documents for the resulting *change* to be reviewed
  (`.doc.product`_).

  Other examples include `.doc.guide`_, manuals, and standards.

_`.doc.product`: Product document
  The document developed from the source documents, and offered for
  review.  The work under review.  The changes under review.  The work
  product.  [Much of this procedure has been rephrased in term of
  reviewing a *change*, since this is a *change review procedure* and
  the tools, such as GitHub, focus on reviewing change.  Introducing a
  new product document is a change.  RB 2023-01-23]

_`.doc.record`: Review records
  Documents produced by the review procedures, which record the
  progress and results of the review.  See `.entry.record`_,
  `.plan.record`_, `.ko.record`_, `.check.record`_, `.log.record`_,
  `.brainstorm.record`_, `.edit.record`_, `.pi.record`_, and
  `.exit.record`_.

  On GitHub, the record of a review can consist of separate comments
  that appear in the pull request, e.g. by checkers submitting GitHub
  reviews.  See also `.doc.log`_.

  In any case, review records must be specific, permanent, and
  referencable.

_`.doc.log`: Issue log
  A record of issues raised during the logging meeting, specifying
  their location, type, finder, and a brief description.

  On GitHub, the issue log includes all GitHub review comments or
  GitHub individual comments that appear in the pull request for the
  change under review.  See also `.doc.record`_.

  Every issue log entry must be specific, permanent, referencable, and
  traceable from `.doc.product`_ and `.doc.rev`_.

_`.doc.rev`: Revised document
  The result of performing the edit procedure on the `.doc.product`_.
  The revised version of the change under review.

_`.doc.acc`: Accepted document
  The result of a Revised document passing exit.  [This isn't
  mentioned.  RB 2023-01-28]

_`.doc.rule`: Rules and rule sets
  A rule or set of rules that `.doc.product`_ should obey.

  Rules are developed by process improvement of the project as a
  whole.  In this procedure, they are updated by `.pi`_ as a result of
  `.brainstorm`_.

  Rule sets are kept short and and rules kept terse to help with
  checking.

_`.doc.guide`: Guides
  A guide that `.doc.product`_ is expected to follow, though not
  strictly.

  Guides are generally longer, more detailed, and more discursive than
  `.doc.rule`_ and contain advice about good practice.  As such, they
  are less useful for review checking than `.doc.rule`_ or
  `.doc.check`_.

  Guides are developed by process improvement of the project as a
  whole.  In this procedure, they are updated by `.pi`_ as a result of
  `.brainstorm`_.

_`.doc.check`: Checklists
  A list of questions to help check against `.doc.rule`_.  A negative
  answer to a checklist question indictes that a rule has been broken.

  Checklists often contain specific questions that can help determine
  whether rules are broken.  For example, a code checklist might say

    .error.check: Are function status/error/exception returns
    checked and acted upon?

  which is ultimately part of a checking generic rule like

    .achieve: A document must achieve (be consistent with) its
    purpose.

  Checklists are developed by process improvement of the project as a
  whole.  In this procedure, they are updated by `.pi`_ as a result of
  `.brainstorm`_.

_`.doc.entry`: Entry criteria
  `.doc.rule`_ that must be met before review to ensure that the
  `.doc.product`_ is likely to pass `.doc.exit`_, so that resources
  are not wasted on a premature review.

_`.doc.exit`: Exit criteria
  `.doc.rule`_ that must be met for `.doc.rev`_ to pass review and be
  approved for use.

_`.doc.proc`: Procedures
  Descriptions of the steps involved in completing any part of process
  (development, review, or otherwise).

_`.doc.imp`: Brainstormed improvement suggestions
  Suggested improvements to process (and hence to some document)
  arising from the process brainstorm.

_`.doc.request`: Requests for change
  An issue that the editor cannot deal with that is escalated to some
  other tracking system, such as a GitHub issue.


7. Calculations
===============

[Update the gender-specific tags in this section.  RB 2023-02-02]

_`.calc`: [Need to mention how this info is used.  Ref kpa.qpm.  RB
2023-01-26]

_`.calc.manpower-used`: The manpower used is the time for entry,
kickoff, checking, logging, brainstorm, edit, and exit.  Kickoff,
checking, logging and brainstorm must be multiplies by the number of
checkers.  Entry and kickoff may be assigned to another document
reviewed at the same time.

_`.calc.manpower-saved`: The default calculation is the number of
major defects found and fixed, multiplies by 10 man-hours.  This
represent the cost of a major defect found by QC.  If the defect would
have reached customers, the estimate should be 100 man-hours.  A
better estimate can be made, with justification.

_`.calc.defects-remaining`: The calculation of defects remaining
should use the estimate <major defects found>/<number of pages>.  The
obvious adjustment must be made for sampling.  The number of
unresolved major issues (raised) should be added.  [In an ideal world,
I believe we should know what proportion of major defects we find, and
use that.  Perhaps we could use 75%? - GavinM] [Doesn't that mean we
could determine whether a document fails review before `.edit`_?  RB
2023-01-28]


8. Checking Roles
=================

["Checking role" is too easily conflated with "review role" and should
perhaps be renamed to "method".  RB 2023-01-23]

_`.role.check`: Checking roles are assigned (`.plan.roles`_) to
`.role.checker`_ in order to focus their attention on different
aspects of the change under review, and so increase the number of
unique major defects found.

_`.role.check.backwards`: The *backwards checking role* involves
scanning the product document in reverse order, in order to increase
the chances of finding major defects that won't be found by other
checkers.  The checker should use their initiative in determining the
granularity of this reversal; for example: in an implementation, the
checker might read each function or type definition in turn from the
end of the file; for other documents, the checker might read each
subsection or paragraph from the end backwards.  For the convenience
of other checkers and the editor, the backwards checker should their
issues in forwards document order.  See `.log.order`_.  [This advice
may no longer be relevant with automated tools.  RB 2023-01-26]

_`.role.check.clarity`: The *clarity checking role* focuses on whether
the product document is clear and obvious.  This is a good role to
give to someone who has never seen the product document before, but
who is in the intended readership.  Anything that is unclear to them
is a defect.

_`.role.check.consistency`: The *consistency checking role* focuses on
whether the product document or documents are internally consistent.

_`.role.check.convention`: The *convention checking role* concentrates
on whether the product document complies with detailed conventions and
rules.

_`.role.check.correctness`: The *correctness checking role* focuses on
whether the product document is correct, i.e. will have the intended
consequences.

_`.role.check.source`: The *source checking role* concentrates on
whether the product document is consistent with any source documents,
and whether dependencies and links are documented where appropriate.

[Other possible checking roles:

  - checking using a different medium (printouts)
  - checking random things in a random order, using dice
  - sampling large or repetitive changes at random
  - build, test, lint, and other automated tools

RB 2023-01-29]


9. Issue Classification
=======================

[Imported from mminfo:guide.review.class and needs updating.  RB
2023-01-26]

_`.class`: There are many possible schemes for defect classification,
but only a coarse one is used here.  Any issue raised, must fall into
one of the following classes.  The normal abbreviation is indicated.

_`.class.major`: (M): A Major defect is a defect in the Product
document that will probably have significantly increased costs to find
and fix later in the development process, for example in testing or in
use ([Gilb-93]_ p442).  A bug that is fixed after review typically
takes one man-hour, after testing 10 man-hour, and in the field 100
man-hours.  A defect that will waste downstream development effort is
also major.  Typical major defects are:

- In an implementation, potentially failing to behave as specified;

- In an implementation, failing to validate foriegn data;

- In a high-level document, being likely to cause major defects in
  derived documents.

_`.class.minor`: (m): A minor defect is any defect in the Product
document whose cost to fix does not increase in time.  If there is a
typo, then it doesn't matter when it's fixed.  Typical minor defects
are:

- an implementation, poor variable names;

- in any human-readable text, typos where the meaning is clear.

_`.class.comment`: (C): A comment is any remark about the product
document.  Typical comments are:

- suggestions for how an algorithm could be optimised in future;

- praise.

_`.class.question`: (q): A question is any matter on which
`.role.checker`_ wants clarification.

- If a product document is unclear to the intended readership then
  that's also `.class.major`_ or `.class.minor`_, by
  `rule.generic.clear`_.

- Questions will be answered in writing (`.edit.act.answer`_).
  Answering them often spawns changes anyway.

- Typical questions are:

  - Clarifications on why things should be the way they are;

  - Curiosity about the details of something.

_`.class.imp`: (I): An improvement suggestion is any potential defect
found in documents other than the product document.  Typical
improvement suggestions are:

- defects in source documents;

- defects in rule sets, check lists, or procedures.

_`.class.new`: (N): Any issue found during logging (as opposed to
during checking) is a new issue.  This classification is orthogonal to
the preceding.  It is important to mark new issues, in order to
measure how worthwhile group logging sessions are (see
`.log.purpose`_).

.. _rule.generic.clear: rule.generic.rst#2


11. Advice for the author
=========================

_`.advice.author`: The intense scrutiny a formal review of your work
can be distressing.  Remember that you are not under attack.  Everyone
is working to make your work *better*.

With that in mind, here is some advice from [Gilb-93]_:

  - Report your own noted issues after giving your team-mates a
    chance.

  - Don't say 'I found that too!'

  - Thank your colleagues for their efforts on your behalf.

  - Learn as much as possible about avoiding the issues as an author.

  - Respect the opinion of your team-mates.  Do not justify or defend.

  - Check the logging for legibility and intelligibility.

  - Answer any 'questions of intent' logged by checkers at the end of
    the logging meeting.


12. Express review
==================

_`.express`: The *express review procedure* [RB-2023-02-01]_ can be
executed by an experienced `.role.leader`_ to get a low-risk change
reviewed quickly, at low cost.

_`.express.readership`: The readership of this section is experienced
review leaders (`.role.leader`_).

_`.express.brief`: If anything in this section is unclear to you,
you're not ready to run express reviews.

_`.express.try`: During an express review, if things go wrong, or turn
out to be riskier or more complicated than you thought, just go back
and `.plan`_ a full review.  Record that you did.  Don't delete the
express review record.

_`.express.record`: Record the express procedure (`.doc.record`_).
You can squash the records for the other steps in one comment.

_`.express.entry`: Execute `.entry`_ pretty much as usual.

_`.express.call`: Call someone else in right now.

_`.express.risk`: The other person must agree that the change has low
risk, and that express review will achieve the `.purpose`_.

- Size is not risk.  It's much more important to consider *what* is
  being changed and *how*.

_`.express.time`: Express review should take no more than about 30
minutes.  If it takes longer, revert to full review.

_`.express.schedule`: No need to schedule. You both do it now.

_`.express.train`: Choo choo!  Don't do this with untrained people.
Revert to full review.

_`.express.source`: All source docs must be immediately available.
If not, you know what to do by now.

_`.express.rule`: Everyone must know the relevant rules.

_`.express.homework`: If homework is needed, it's not an express
review.

_`.express.remind`: Remind everyone of the `.purpose`_ of review.

_`.express.role`: Everyone will perform every `.role.check`_.  Not
feasible?  It's not an express review.

_`.express.improve`: Express reviews don't support extra objectives.

_`.express.major`: If anyone finds `major defects`_, stop the express
review and `.plan`_ a full one.

_`.express.check`: Do separate checking for some minutes.  Look for
`major defects`_, note other issues.  Don't confer.

_`.express.log`: Confer.  Announce issues, look for `major defects`_,
note other issues.

_`.express.log.proper`: You still need to record issues properly, even
in an express review.  Don't know how?  You're not ready to run an
express review.

_`.express.brainstorm`: Take a one minute break after logging then do
a few minutes of brainstorm.  Prevention is still a goal.

_`.express.edit`: If there are just a few minor edits, do them now,
together (like `pair programming`_).  Otherwise, drop out of express
review into `.edit`_.  Record this decision, natch.

.. _pair programming: https://en.wikipedia.org/wiki/Pair_programming

_`.express.pi`: Defer/delegate `.pi`_ but don't drop it.  Prevention
is worth it.

_`.express.exit`: Execute `.exit`_ pretty much as usual.  Do record
metrics.


13. Rationale
=============

Formal review is the key to the quality of the Memory Pool System.

A full justification of the review process described by this procedure
is not feasible here.  There are three sources:

1. the process improvement history of the Memory Pool System project,

2. Software Inspection [Gilb-93]_,

3. the analysis work behind the Capability Maturity Model
   [CMU-SEI-93-TR-024]_.

Of these, (1) is unfortunately the least accessible, because the
documents have travelled through several different systems, and
version control did not always survive.

Ravenbrook does have hundreds of archived review records [MM-reviews]_
with estimates of review productivity (produced by
`.phase.estimation`_).  [At some point it would be good to summarize
those here.  RB 2023-01-28]


13.1. Why formal reviews?
-------------------------

Every formal review has been worthwhile in terms of preventing defects
versus the cost of review.

The Harlequin MM Group adopted code review in the mid 1990s -- early
compared to most of the industry.  Casual code reviews (where someone
eyeballs diffs) have become standard practice for many projects, and
it's quite hard to imagine a time without them.  However, full-on
formal reviews or inspections are still relatively rare.

Formal review is appropriate for the MPS because defects in memory
managers, and especially in garbage collectors, are *extremely*
expensive to find and fix compared to other software.

It's the job of a garbage collector to destroy information by
recycling (overwriting) objects and reorganizing memory.  A subtle
failure of GC logic can cause a failure in the client software many
hours later.  When that failure happens to a user of an application
delivered by developers using a compiler developed by your client that
uses the MPS in its runtime system, well, forget about it.  A defect
in the compiler (usually considered expensive) is relatively cheap!

This means that the cost of `major defects`_ escalates *much* more
steeply for the MPS than most software, so it is especially worthwhile
to catch them early in the development process.

Even testing is too late.


A. References
=============

.. [CMU-SEI-93-TR-024] "Capability Maturity Model for Software,
		       Version 1.1"; Mark C. Paulk, Bill Curtis, Mary
		       Beth Chrissis, Charles V. Weber; Software
		       Engineering Institute, Carnegie Mellon
		       University; 1993-02;
		       <https://resources.sei.cmu.edu/library/asset-view.cfm?assetid=11955>.

.. [CMU-SEI-93-TR-025] "Key Practices of the Capability Maturity
                       Model, Version 1.1"; Mark C. Paulk,
                       Charles V. Weber, Suzanne M. Garcia, Mary Beth
                       Chrissis, Marilyn Bush; Software Engineering
                       Institute, Carnegie Mellon University; 1993-02;
                       <https://resources.sei.cmu.edu/asset_files/TechnicalReport/1993_005_001_16214.pdf>.

.. [Gilb-93] "Software Inspection"; Tom Gilb, Dorothy Graham; Addison
             Wesley; 1993; ISBN 0-201-63181-4; book.gilb93.

.. [MM-guide.review.edit] "Guidelines for review edits"; Gavin
			  Matthews; Harlequin Limited; 1996-10-31;
			  mminfo:guide.review.edit;
			  //info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mminfo/guide/review/edit/index.txt#1.

.. [MM-process.review] "The review process"; Richard Brooksby;
		       Harlequin Limited; 1995-08-18;
		       mminfo:process.review;
		       //info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mminfo/process/review/index.txt#1.

.. [MM-proc.review.brainstorm] "Procedure for process brainstorm in
			       review"; Gavin Matthews; Harelquin
			       Limited; 1997-06-12;
			       mminfo:proc.review.brainstorm;
			       //info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mminfo/proc/review/brainstorm/index.txt#1.

.. [MM-proc.review.check] "Procedure for checking in review"; Gavin
			  Matthews; Harlequin Limited; 1997-06-12;
			  mminfo:proc.review.check;
			  //info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mminfo/proc/review/check/index.txt#1.

.. [MM-proc.review.entry] "Procedure for review entry"; Gavin
			  Matthews; Harlequin Limited; 1997-06-02; mminfo:proc.review.entry;
			  //info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mminfo/proc/review/entry/index.txt#1.

.. [MM-proc.review.exit] "Procedure for exiting a document from
			 review"; Gavin Matthews; Harlequin Limited;
			 1997-06-12; mminfo:proc.review.exit;
			 //info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mminfo/proc/review/exit/index.txt#1.

.. [MM-proc.review.ko] "Procedure for a review kickoff meeting"; Gavin
		       Matthews; Harlequin Limited; 1997-06-12;
		       mminfo:proc.review.ko;
		       //info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mminfo/proc/review/ko/index.txt#1.

.. [MM-proc.review.log] "Procedure for review logging meeting"; Gavin
			Matthews; Harlequin Limited; 1997-06-12;
			mminfo:proc.review.log;
			//info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mminfo/proc/review/log/index.txt#1

.. [MM-reviews] Review records of the MM Group; Harlequin Limited;
		mminfo:review.*;
		//info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mminfo/review/...

.. [RB-2023-02-01] "Express review notes and test"; Richard Brooksby;
		   Ravenbrook Limited; 2023-02-01;
		   <https://info.ravenbrook.com/mail/2023/02/01/20-06-44/0/>.


B. Document History
===================

==========  =====  ==================================================
2023-01-19  RB_    Created.
2023-01-20  RB_    Importing material from MM Group proc.review.
2023-01-26  RB_    Importing checking roles and issue classification
                   from MM Group documents.
2023-01-28  RB_    Developing the Rationale.
                   Tidying up remaining comments.
                   Revising entry, planning, kickoff, and exit.
                   Revising documents section.
2023-01-30  RB_    Revising checking, logging, and brainstorm.
2023-01-31  RB_    Revised based on `review test run`_.
2023-02-01  RB_    Implementing `.express`_.
==========  =====  ==================================================

.. _RB: mailto:rb@ravenbrook.com

.. _review test run: https://github.com/Ravenbrook/mps/pull/123#issuecomment-1408682681


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
