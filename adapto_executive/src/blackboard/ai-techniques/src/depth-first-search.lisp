(in-package "AIT")

(defclass search-problem ()
  ( (initial-state :initform nil :initarg :initial-state :accessor initial-state)
    (goal-test :initform nil :initarg :goal-test :accessor goal-test) ; Funktion
    (expand-fn :initform nil :initarg :expand-fn :accessor expand-fn)
    (problem-solution :initform nil :accessor problem-solution) ))

;; zum Testen
(defparameter prob (make-instance 'search-problem
                    :initial-state 'a
                    :goal-test #'(lambda (s) (equal s 'l))
                    :expand-fn #'(lambda (s) (case s
                                               (a '(b c))
                                               (b '(d e))
                                               (c '(f g))
                                               (d '(h i))
                                               (e '(j k))
                                               (f '(l m))
                                               (g '(n o p))
                                               (otherwise ())))))



;; States
(defclass state ()
  ( (state-description :initform nil :initarg :state-description :reader state-description)
    (level :initform 0 :initarg :level :reader level)
    (path :initform () :initarg :path :accessor path) ))

(defclass solution (state) ())

;; Processes
(defclass expand-process (bb:process) ())
(defclass action-process (bb:process) ())


;; Search Algorithm
(let ( (bb (bb:make-blackboard))
       (problem nil) )

  (defun init-processes ()
    (let ( (expand-proc (make-instance 'expand-process))
           (action-proc (make-instance 'action-process)) )
      (bb:register-process expand-proc 'state bb)
      (bb:register-process action-proc 'solution bb)
      (values expand-proc action-proc)))

  ;; producer-process
  (defmethod process-run ((proc expand-process))
    (labels ( (collect-states-from-queue (&optional (collected-states nil) (max-level-states 0))
                (cond ( (bb:chunk-queue-empty-p proc)
                        (values collected-states max-level-states) )
                      ( T
                        (let ( (next-state-chunk (bb:pop-chunk-queue proc)) )
                          (collect-states-from-queue
                            (cons next-state-chunk collected-states)
                            (if (> (level (data next-state-chunk)) max-level-states)
                              (level (data next-state-chunk))
                              max-level-states))) )))
              (choose-state (available-state-chunks max-level)
                (cond ( (null available-state-chunks)
                        nil ) ; dieser Fall kann eigentlich nicht eintreten
                      ( (= (level (data (first available-state-chunks))) max-level)
                        (values (data (first available-state-chunks)) (first available-state-chunks)) )
                      ( T
                        (choose-state (rest available-state-chunks) max-level) ))) )
      (format t "expand process~%")
      (format t "chunk-queue: ~a~%" (mapcar #'state-description (mapcar #'data (bb:get-chunk-queue proc))))
      (cond ( (bb:chunk-queue-empty-p proc)
              (bb:backtrack bb)
              (format t "backtracking~%")
              (sleep 0.1) ; eigentlich nicht nötig, aber sonst sieht man Ausgaben nicht
              (process-run proc) )
            ( T
              (multiple-value-bind (available-state-chunks level) (collect-states-from-queue)
                (multiple-value-bind (current-state current-state-chunk) (choose-state available-state-chunks level)
                  (cond ( (funcall (goal-test problem) (state-description current-state))
                          (bb:add-chunk-to-bb
                            (make-instance 'bb:chunk :data (make-instance 'solution
                                                             :state-description (state-description current-state)
                                                             :path (path current-state)
                                                             :level (level current-state)))
                            bb)
                          (sb-thread:terminate-thread sb-thread:*current-thread*)); Prozess stoppt
                        ( T
                          (remove-chunk-from-bb current-state-chunk bb)
                          (bb:set-backtracking-point bb)
                          (mapcar #'(lambda (new-state)
                                      (bb:add-chunk-to-bb (make-instance 'bb:chunk
                                                            :data (make-instance 'state
                                                                    :state-description new-state
                                                                    :path (cons (state-description current-state)
                                                                                (path current-state))
                                                                    :level (1+ (level current-state))))
                                                          bb))
                                  (funcall (expand-fn problem) (state-description current-state)))
                          (sleep 0.1)
                          (process-run proc) )))) ))))

  ;; action process
  (defmethod process-run ((proc action-process))
    (format t "action process~%")
    (cond ( (bb:chunk-queue-empty-p proc)
            (sleep 0.5); eigentlich nicht nötig, aber sonst sieht man Ausgaben nicht
            (process-run proc) )
          ( T
            (setf (problem-solution problem) (bb:data (bb:pop-chunk-queue proc)))
            (format t "solution found: ~a~%" (state-description (problem-solution problem)))
            (sb-thread:terminate-thread sb-thread:*current-thread*) )))

  (defun set-problem (new-problem)
    (setf problem new-problem))

  (defun depth-first-search ()
    (multiple-value-bind (expand-proc action-proc) (init-processes)
      (sb-thread:make-thread #'(lambda () (process-run expand-proc)) :name "expand")
      (bb:add-chunk-to-bb (make-instance 'bb:chunk
                            :data (make-instance 'state :state-description (initial-state problem)
                                                        :level 0))
                          bb)
      (sb-thread:make-thread #'(lambda () (process-run action-proc)) :name "action")))

)
