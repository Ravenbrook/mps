;;; -*- Package: C -*-
;;;
;;; Load this file to profile the CMUCL compiler. 


(in-package "C")

(use-package "PROFILE")
(declaim (ftype (function (t t t) *) ir1-top-level))
(declaim (ftype (function (t) *) find-initial-dfo))
(declaim (ftype (function (t) *) find-dfo))
(declaim (ftype (function (t) *) local-call-analyze))
(declaim (ftype (function (t) *) delete-block))
(declaim (ftype (function (t) *) join-successor-if-possible))
(declaim (ftype (function (t) *) ir1-optimize))
(declaim (ftype (function (t) *) ir1-optimize-block))
(declaim (ftype (function (t) *) flush-dead-code))
(declaim (ftype (function (t) *) generate-type-checks))
(declaim (ftype (function (t) *) constraint-propagate))
(declaim (ftype (function (t) *) pre-environment-analyze-top-level))
(declaim (ftype (function (t) *) environment-analyze))
(declaim (ftype (function (t) *) gtn-analyze))
(declaim (ftype (function (t t) *) control-analyze))
(declaim (ftype (function (t) *) ltn-analyze))
(declaim (ftype (function (t) *) stack-analyze))
(declaim (ftype (function (t) *) ir2-convert))
(declaim (ftype (function (t) *) copy-propagate))
(declaim (ftype (function (t) *) select-representations))
(declaim (ftype (function (t) *) lifetime-analyze))
(declaim (ftype (function (t) *) delete-unreferenced-tns))
(declaim (ftype (function (t) *) pack))
(declaim (ftype (function (t) *) generate-code))
(declaim (ftype (function (t t t t t t) *) fasl-dump-component))
(declaim (ftype (function (t) *) debug-info-for-component))
(declaim (ftype (function (t) *) macerate-ir1-component))
(declaim (ftype (function (t) *) merge-top-level-lambdas))
(declaim (ftype (function (t t) *) note-failed-optimization))
(declaim (ftype (function (&optional t) *) clear-stuff))
(declaim (ftype (function (t t) *) fasl-dump-source-info))
(declaim (ftype (function (t t) *) fasl-dump-top-level-lambda-call))
(declaim (ftype (function (t t) *) new-assem:append-segment))
(declaim (ftype (function (t) *) new-assem:finalize-segment))
(declaim (ftype (function (t) *) new-assem::schedule-pending-instructions))

(profile new-assem:append-segment
	 new-assem:finalize-segment
	 new-assem::schedule-pending-instructions
	 ir1-top-level
	 find-initial-dfo
	 find-dfo
	 local-call-analyze
	 delete-block
	 ir1-optimize
	 join-successor-if-possible
	 ir1-optimize-block
	 flush-dead-code
	 generate-type-checks
	 constraint-propagate
	 pre-environment-analyze-top-level
	 environment-analyze
	 gtn-analyze
	 control-analyze
	 ltn-analyze
	 stack-analyze
	 ir2-convert
	 copy-propagate
	 select-representations

	 lifetime-analyze
#|
	 lifetime-pre-pass
	 lifetime-flow-analysis
	 reset-current-conflict
	 lifetime-post-pass
|#
	 delete-unreferenced-tns
	 pack
#|
	 pack-wired-tn
	 pack-tn
	 pack-load-tns
	 assign-tn-costs
	 optimized-emit-saves
	 emit-saves
|#
	 generate-code
	 debug-info-for-component
	 fasl-dump-component
	 macerate-ir1-component
	 merge-top-level-lambdas
	 note-failed-optimization
	 clear-stuff
	 read-source-form
	 fasl-dump-source-info
	 fasl-dump-top-level-lambda-call
;	 check-life-consistency
;	 check-ir1-consistency
;	 check-ir2-consistency
	 )
