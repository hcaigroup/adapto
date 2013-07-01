(in-package "TSP")

;; Werte für Wartezeiten: an manchen Stellen ist warten nötig, damit direction expert zum Zug kommt
;; mit den aktuellen Werten funktioniert es, ist aber nicht genauer untersucht

(defclass action-process (coordinator) ())

(let ( (evaluated-candidate-points nil) )

  (defun choose-next-point ()
    (labels ( (calculate-average-evaluation-value (cp)
              (/ (reduce #'+ (evaluation-values cp) :key #'cdr)
                 (length (evaluation-values cp)))) )
      (let ( (best-candidate-point (first evaluated-candidate-points))
             (best-candidate-point-value
               (calculate-average-evaluation-value (first evaluated-candidate-points))) )
;        (format t "evaluated points~%")
;        (format t "point: ~a, eval values: ~a, average: ~a~%"
;          (id (first evaluated-candidate-points))
;          (evaluation-values (first evaluated-candidate-points))
;          (calculate-average-evaluation-value (first evaluated-candidate-points)))
        (dolist (ii (rest evaluated-candidate-points))
;          (format t "point: ~a, eval values: ~a, average: ~a~%"
;            (id ii)
;            (evaluation-values ii)
;            (calculate-average-evaluation-value ii))
          (let ( (cp-evaluation-value (calculate-average-evaluation-value ii)) )
            (when (> cp-evaluation-value best-candidate-point-value)
              (setf best-candidate-point ii)
              (setf best-candidate-point-value cp-evaluation-value))))
              (let*( (current-problem (get-current-state))
                     (new-problem (make-instance 'problem
                                    :name (name current-problem)
                                    :start-point best-candidate-point
                                    :end-point (end-point current-problem)
                                    :other-points
                                      (remove best-candidate-point (other-points current-problem)
                                        :test #'(lambda (a b) (= (id a) (id b))))
                                    :dimension (1- (dimension current-problem))
                                    :variant (variant current-problem)
                                    :solution (cons (start-point current-problem)
                                                    (solution current-problem)))) )
                (setf (available-p best-candidate-point) nil)
                (dolist (pt (other-points new-problem))
                  (setf (evaluation-values pt) ()))
                (set-current-state new-problem)
                (setf evaluated-candidate-points nil)))))

  (defun create-solution-and-stop-processes ()
    (let ( (problem (get-current-state)) )
      (setf (solution problem)
            (reverse (cons (end-point problem)
                     (cons (start-point problem) (solution problem))))))
    (format t "gefundene Loesung: ~a (~a)~%"
      (mapcar #'id (solution (get-current-state)))
      (evaluate-problem-solution (get-current-state)))
    (bb:add-chunk-to-bb (make-instance 'chunk :data (make-stop-signal)))
    (setf *problem-solved-p* T))


  (defmethod process-run ((proc action-process))
;    (format t "process action-process~%")
    (cond ( (bb:chunk-queue-empty-p proc)
            (sleep 0.001)
            (process-run proc) )
          ( T
            (let ( (new-chunk-data (data (bb:pop-chunk-queue proc))) )
;              (format t "evaluated-candidate-points: ~a ~a (~a)~%"
;                evaluated-candidate-points
;                (mapcar #'id evaluated-candidate-points)
;                (mapcar #'id (other-points (get-current-state))))
              (cond ( (typep new-chunk-data 'candidate-point)
;                      (format t "process action-process working~%")
                      (when (evaluation-values new-chunk-data)
                        (pushnew new-chunk-data evaluated-candidate-points))
                      ; wenn sich zwischendurch der Evaluationswert geändert hat, dann ist das in der Liste schon aktuell, weil dort Objekte gespeichert sind
                      (cond ( (>= (length evaluated-candidate-points) (1- (dimension (get-current-state))))
                              ; Entscheidung für einen Punkt und Änderung des aktuellen Zustandes/Problems
                              (choose-next-point)
                              ; Prüfen ob Problem gelöst ist
                              (cond ( (<= (dimension (get-current-state)) 1)
                                      (create-solution-and-stop-processes) )
                                    ( T
                                      (bb:wipe-blackboard)
                                      (bb:add-chunk-to-bb (make-instance 'chunk :data (make-next-signal)))
                                      (sleep 0.001)
                                      (process-run proc) )) )
                           ( T ; noch nicht alle Punkte bewertet
                             (sleep 0.001)
                             (process-run proc) )) )
                    ( T ; kein interessanter chunk-type in Warteschlange
                      (sleep 0.001)
                      (process-run proc) ))) )))

; allgemein auch möglich: backtracking-Punkte setzen und backtracking anstoßen
)


