(in-package "TSP")

(defparameter *problem-solved-p* nil)

(defparameter *experts* `(
;; direction
(nn-direction-expert (process-signal) :number-of-neighbors 5)
;; evaluation
(nn-single-evaluation-expert (candidate-point))
;(nn-all-evaluation-expert (ranking))
;; coordination
;(ranking-process (candidate-point))
(action-process (candidate-point ranking))
))



; Zustand = remaining-problem
; Zustand als globale Information will ich nicht auf dem Blackboard haben
(let ( (current-state nil) )
  (defun get-current-state ()
    current-state)

  (defun set-current-state (new-state)
    (setf current-state new-state))
)

(defun kill-thread (thread)
  (when (sb-thread:thread-alive-p thread)
    (handler-case
        (prog1 t (sb-thread:terminate-thread thread))
      (error () nil))))

;; spezieller Prozess um am Ende noch laufende Prozesse zu beenden
(defun check-end-of-computation (stop-process expert-processes)
  (cond ( (chunk-queue-empty-p stop-process)
          (sleep 0.1)
          (check-end-of-computation stop-process expert-processes) )
        ( T
          (dolist (thread expert-processes)
            (kill-thread thread)) )))


;; Hauptfunktion
(defun solve-tsp (problem)
  ; reset von möglichen vorherigen Läufen
  (setf *problem-solved-p* nil)
  (delete-global-blackboard)
  (setf (solution problem) nil)
  (dolist (pt (cons (start-point problem) (other-points problem)))
    (when (typep pt 'candidate-point)
      (setf (evaluation-values pt) ()))
    (setf (available-p pt) T))
  ; Lösung des Problems
  (set-current-state problem)
  (make-blackboard :use-as-global-bb T)
  (let ( (expert-processes (mapcar #'(lambda (expert)
                                     (destructuring-bind (expert-class chunk-classes &rest expert-args) expert
                                       (let ( (expert-proc (apply #'make-instance expert-class expert-args)) )
                                         (mapcar #'(lambda (chunk-type)
                                                   (bb:register-process expert-proc chunk-type))
                                                 chunk-classes)
                                         expert-proc)))
                                   *experts*)) )
;    (dolist (proc expert-processes)
;      (sb-thread:make-thread #'(lambda () (process-run proc))))

    (let ( (expert-threads ()) )
      (dolist (proc expert-processes)
        (push (sb-thread:make-thread #'(lambda () (process-run proc))) expert-threads))
      (let ( (stop-process (make-instance 'bb:process)) )
        (bb:register-process stop-process 'stop-signal)
        (sb-thread:make-thread #'(lambda () (check-end-of-computation stop-process expert-threads)) :name "stop")))
    ; Prozess zum Debuggen
;    (sb-thread:make-thread #'(lambda ()
;      (loop
;        (format t "=============== STATUS ===============~%")
;        (format t "blackboard: ~{~a~^~%~}~%" (multiple-value-list (bb:show-blackboard)))
;        (format t "aktuelle Problemdimension: ~a~%" (dimension (get-current-state)))
;        (format t "aktueller Startpunkt: ~a~%" (start-point (get-current-state)))
;        (format t "aktueller Endpunkt: ~a~%" (end-point (get-current-state)))
;        (format t "aktuelle Restpunkte: ~a~%" (other-points (get-current-state)))
;        (format t "aktuelle Loesung: ~a~%" (solution (get-current-state)))
;        (format t "======================================~%")
;        (sleep 0.5))))
    ; Signal für direction experts zum Loslegen
    (bb:add-chunk-to-bb (make-instance 'chunk :data (make-next-signal)))
    ; warten bis Prozesse fertig sind und dann Ergebnis zurückliefern
    ; ist keine schöne Lösung, aber war erstmal am schnellsten zu implementieren
    (loop
      (if *problem-solved-p*
        (return)
        (sleep 0.1)))
    (setf (solution problem) (solution (get-current-state)))
    (print-tsp problem)))
;    (solution (get-current-state))))

