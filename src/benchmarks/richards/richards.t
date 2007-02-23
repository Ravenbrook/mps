(herald richards)

(define deviceA 5)
(define deviceB 6)
(define devicePacketKind 1)
(define handlerA 3)
(define handlerB 4)
(define idler 1)
(define noWork nil)
(define noTask nil)
(define worker 2)
(define workPacketKind 2)

(lset taskList noTask)
(lset currentTask nil)
(lset currentTaskIdentity nil)
(lset taskTable (make-vector 6))
(lset tracing nil)
(lset layout 0)
(lset queuePacketCount 0)
(lset holdCount 0)

(define-operation (run self work))

(vector-fill taskTable noTask)

(define (hash-obj l)
  (if l (object-hash l) nil))

(define-structure-type taskControlBlock
  packetPending taskWaiting taskHolding link identity priority input state
  handle)

(define-structure-type packet
  link identity kind datum data)

(define-structure-type deviceTaskDataRecord
  pending
  (((run self work)
    (lset functionWork work)
    (if (equal? noWork functionWork)
	(block
	 (set functionWork (deviceTaskDataRecord-pending self))
	 (if (equal? noWork functionWork)
	     (wait)
	     (block
	      (set (deviceTaskDataRecord-pending self) noWork)
	      (queuePacket functionWork))))
	(block
	 (set (deviceTaskDataRecord-pending self) functionWork)
	 (if tracing (trace (packet-datum functionWork)))
	 (holdSelf))))))

(define-structure-type handlerTaskDataRecord
  workIn deviceIn
  (((run self work)
    (if (equal? noWork work)
	nil
	(if (equal? workPacketKind (packet-kind work))
	    (workInAdd self work)
	    (deviceInAdd self work)))
    (let ((workPacket (handlerTaskDataRecord-workIn self)))
      (if (equal? noWork workPacket)
	  (wait)
	  (let ((count (packet-datum workPacket)))
	    (if (> count 4)
		(block
		 (set (handlerTaskDataRecord-workIn self)
		      (packet-link workPacket))
		 (queuePacket workPacket))
		(let ((devicePacket (handlerTaskDataRecord-deviceIn self)))
		  (if (equal? noWork devicePacket)
		      (wait)
		      (block
		       (set (handlerTaskDataRecord-deviceIn self)
			    (packet-link devicePacket))
		       (set (packet-datum devicePacket)
			    (vref (packet-data workPacket) (- count 1)))
		       (set (packet-datum workPacket) (+ count 1))
		       (queuePacket devicePacket)))))))))))

(define-structure-type idleTaskDataRecord
  control count
  (((run self work)
    (ignorable work)
    (set (idleTaskDataRecord-count self)
	 (- (idleTaskDataRecord-count self) 1))
    (if (equal? 0 (idleTaskDataRecord-count self))
	(holdSelf)
	(if (equal? 0 (logand (idleTaskDataRecord-control self) 1))
	    (block
	     (set (idleTaskDataRecord-control self)
		  (quotient (idleTaskDataRecord-control self) 2))
	     (release deviceA))
	    (block
	     (set (idleTaskDataRecord-control self)
		  (logxor (quotient (idleTaskDataRecord-control self) 2)
			  53256))
	     (release deviceB)))))))

(define-structure-type workerTaskDataRecord
  destination count
  (((run self work)
    (if (equal? noWork work)
	(wait)
	(block
	 (set (workerTaskDataRecord-destination self)
	      (if (equal? handlerA (workerTaskDataRecord-destination self))
		  handlerB
		  handlerA))
	 (set (packet-identity work) (workerTaskDataRecord-destination self))
	 (set (packet-datum work) 1)
	 (do ((i 0 (+ i 1)))
	     ((> i 3) nil)
	   (set (workerTaskDataRecord-count self)
		(+ (workerTaskDataRecord-count self) 1))
	   (if (> (workerTaskDataRecord-count self) 256)
	       (set (workerTaskDataRecord-count self) 1))
	   (vset (packet-data work) i
		 (+ (char->ascii #\A)
		    (- (workerTaskDataRecord-count self) 1))))
	 (queuePacket work))))))

(define (appendHead packet queueHead)
  (set (packet-link packet) noWork)
  (if (equal? noWork queueHead)
      packet
      (block
       (lset mouse queueHead)
       (lset link (packet-link mouse))
       (do ()
	   ((equal? noWork link) nil)
	 (set mouse link)
	 (set link (packet-link mouse)))
       (set (packet-link mouse) packet)
       queueHead)))

(define (initialize-globals)
  (set taskList noTask)
  (set currentTask nil)
  (set currentTaskIdentity nil)
  (set taskTable (make-vector 6))
  (set tracing nil)
  (set layout 0)
  (set queuePacketCount 0)
  (set holdCount 0)
  (vector-fill taskTable noTask))

(define (richards)
  (initialize-globals)

  (createIdler idler 0 noWork (running (make-taskControlBlock)))

  (lset workQ (createPacket noWork worker workPacketKind))
  (set workQ (createPacket workQ worker workPacketKind))
  (createWorker worker 1000 workQ (waitingWithPacket))

  (set workQ (createPacket noWork deviceA devicePacketKind))
  (set workQ (createPacket workQ deviceA devicePacketKind))
  (set workQ (createPacket workQ deviceA devicePacketKind))
  (createHandler handlerA 2000 workQ (waitingWithPacket))

  (set workQ (createPacket noWork deviceB devicePacketKind))
  (set workQ (createPacket workQ deviceB devicePacketKind))
  (set workQ (createPacket workQ deviceB devicePacketKind))
  (createHandler handlerB 3000 workQ (waitingWithPacket))

  (createDevice deviceA 4000 noWork (waiting))
  (createDevice deviceB 5000 noWork (waiting))

  (schedule)

  (if (not (and (equal? queuePacketCount 23246) (equal? holdCount 9297)))
      (error "richards results incorrect"))

  nil)

(define (schedule)
  (set currentTask taskList)
  (do ()
      ((equal? noTask currentTask) nil)
    (if (isTaskHoldingOrWaiting currentTask)
	(set currentTask (taskControlBlock-link currentTask))
	(block
	 (set currentTaskIdentity (taskControlBlock-identity currentTask))
	 (if tracing (trace currentTaskIdentity))
	 (set currentTask (runTask currentTask))))))

(define (findTask identity)
  (let ((tk (vref taskTable (- identity 1))))
    (if (equal? noTask tk) (error "findTask failed"))
    tk))

(define (holdSelf)
  (set holdCount (+ holdCount 1))
  (set (taskControlBlock-taskHolding currentTask) t)
  (taskControlBlock-link currentTask))

(define (queuePacket packet)
  (let ((tk (findTask (packet-identity packet))))
    (if (equal? noTask tk)
	noTask
	(block
	 (set queuePacketCount (+ queuePacketCount 1))
	 (set (packet-link packet) noWork)
	 (set (packet-identity packet) currentTaskIdentity)
	 (addInput tk packet currentTask)))))

(define (release identity)
  (let ((tk (findTask identity)))
    (if (equal? noTask tk)
	noTask
	(block
	 (set (taskControlBlock-taskHolding tk) nil)
	 (if (> (taskControlBlock-priority tk)
		(taskControlBlock-priority currentTask))
	     tk
	     currentTask)))))

(define (trace id)
  (set layout (- layout 1))
  (if (>= 0 layout)
      (block
       (format (debug-output) "~%")
       (set layout 30)))
  (format (debug-output) "~a " id))

(define (wait)
  (set (taskControlBlock-taskWaiting currentTask) t)
  currentTask)

(define (createDevice identity priority work state)
  (let ((data (create-deviceTaskDataRecord)))
    (createTask identity priority work state data)))

(define (createHandler identity priority work state)
  (let ((data (create-handlerTaskDataRecord)))
    (createTask identity priority work state data)))

(define (createIdler identity priority work state)
  (let ((data (create-idleTaskDataRecord)))
    (createTask identity priority work state data)))

(define (createWorker identity priority work state)
  (let ((data (create-workerTaskDataRecord)))
    (createTask identity priority work state data)))

(define (createTask identity priority work state data)
  (let ((tk (create-taskControlBlock
	     taskList identity priority work state data)))
    (set taskList tk)
    (vset taskTable (- identity 1) tk)))

(define (createPacket link identity kind)
  (create-packet link identity kind))

(define (running tcb)
  (set (taskControlBlock-packetPending tcb) nil)
  (set (taskControlBlock-taskWaiting tcb) nil)
  (set (taskControlBlock-taskHolding tcb) nil)
  tcb)

(define (waiting)
  (let ((tcb (make-taskControlBlock)))
    (set (taskControlBlock-packetPending tcb) nil)
    (set (taskControlBlock-taskWaiting tcb) t)
    (set (taskControlBlock-taskHolding tcb) nil)
    tcb))

(define (waitingWithPacket)
  (let ((tcb (make-taskControlBlock)))
    (set (taskControlBlock-packetPending tcb) t)
    (set (taskControlBlock-taskWaiting tcb) t)
    (set (taskControlBlock-taskHolding tcb) nil)
    tcb))

(define (isTaskHoldingOrWaiting tcb)
  (or (taskControlBlock-taskHolding tcb)
      (and (not (taskControlBlock-packetPending tcb))
	   (taskControlBlock-taskWaiting tcb))))

(define (isWaitingWithPacket tcb)
  (and (taskControlBlock-packetPending tcb)
       (and (taskControlBlock-taskWaiting tcb)
	    (not (taskControlBlock-taskHolding tcb)))))

(define (packetNowPending tcb)
  (set (taskControlBlock-packetPending tcb) t)
  (set (taskControlBlock-taskWaiting tcb) nil)
  (set (taskControlBlock-taskHolding tcb) nil)
  tcb)

(define (create-taskControlBlock
	 link identity priority initialWorkQueue initialState privateData)
  (let ((r (make-taskControlBlock)))
    (set (taskControlBlock-link r) link)
    (set (taskControlBlock-identity r) identity)
    (set (taskControlBlock-priority r) priority)
    (set (taskControlBlock-input r) initialWorkQueue)
    (set (taskControlBlock-packetPending r)
	 (taskControlBlock-packetPending initialState))
    (set (taskControlBlock-taskWaiting r)
	 (taskControlBlock-taskWaiting initialState))
    (set (taskControlBlock-taskHolding r)
	 (taskControlBlock-taskHolding initialState))
    (set (taskControlBlock-handle r) privateData)
    (set (taskControlBlock-state r) nil)
    r))

(define (addInput tcb packet oldTask)
  (if (equal? noWork (taskControlBlock-input tcb))
      (block
       (set (taskControlBlock-input tcb) packet)
       (set (taskControlBlock-packetPending tcb) t)
       (if (> (taskControlBlock-priority tcb)
	      (taskControlBlock-priority oldTask))
	   tcb
	   oldTask))
      (block
       (set (taskControlBlock-input tcb)
	    (appendHead packet (taskControlBlock-input tcb)))
       oldTask)))

(define (runTask tcb)
  (lset message nil)
  (if (isWaitingWithPacket tcb)
      (block
       (set message (taskControlBlock-input tcb))
       (set (taskControlBlock-input tcb) (packet-link message))
       (if (equal? noWork (taskControlBlock-input tcb))
	   (running tcb)
	   (packetNowPending tcb)))
      (set message noWork))
  (run (taskControlBlock-handle tcb) message))

(define (create-packet link identity kind)
  (let ((p (make-packet)))
    (set (packet-link p) link)
    (set (packet-identity p) identity)
    (set (packet-kind p) kind)
    (set (packet-datum p) 1)
    (let ((v (make-vector 4)))
      (vector-fill v 0)
      (set (packet-data p) v))
    p))

(define (create-deviceTaskDataRecord)
  (let ((tk (make-deviceTaskDataRecord)))
    (set (deviceTaskDataRecord-pending tk) noWork)
    tk))

(define (create-handlerTaskDataRecord)
  (let ((tk (make-handlerTaskDataRecord)))
    (set (handlerTaskDataRecord-workIn tk) noWork)
    (set (handlerTaskDataRecord-deviceIn tk) noWork)
    tk))

(define (deviceInAdd tk packet)
  (set (handlerTaskDataRecord-deviceIn tk)
       (appendHead packet (handlerTaskDataRecord-deviceIn tk)))
  tk)

(define (workInAdd tk packet)
  (set (handlerTaskDataRecord-workIn tk)
       (appendHead packet (handlerTaskDataRecord-workIn tk)))
  tk)

(define (create-idleTaskDataRecord)
  (let ((tk (make-idleTaskDataRecord)))
    (set (idleTaskDataRecord-control tk) 1)
    (set (idleTaskDataRecord-count tk) 10000)
    tk))

(define (create-workerTaskDataRecord)
  (let ((tk (make-workerTaskDataRecord)))
    (set (workerTaskDataRecord-destination tk) handlerA)
    (set (workerTaskDataRecord-count tk) 0)
    tk))
