===============================
Exit criteria for all documents
===============================

:tag: exit.universal
:type: rule
:status: incomplete
:author: Gavin Matthews
:date: 1996-08-08

_`.scope`: These criteria should be used for the exit from review of
all documents.  See book.gilb93, page 202.

_`.edit`: The editor has taken written action on all issues, recorded
in the review record; these may include rejecting the issue.

_`.quest`: All questions to the author (q) should have been answered
in mail to mm, and possibly in documentation.
 
_`.imp`: All improvement suggestions (I) should have resulted in one
of the following:

- _`.imp.edit`: Edit of another document; not if it is already
  approved.

- _`.imp.mail`: Passed on to someone responsible for the other
  document, and accepted.

- _`.imp.issue`: Escalated to an InfoSys issue.

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
