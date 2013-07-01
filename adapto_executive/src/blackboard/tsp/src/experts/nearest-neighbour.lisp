(in-package "TSP")

;; Werte für Wartezeiten: an manchen Stellen ist warten nötig, damit direction expert zum Zug kommt
;; mit den aktuellen Werten funktioniert es, ist aber nicht genauer untersucht

;;; sonstige Probleme:
;; bei größeren Problemen (ab 20 Punkten) funktioniert Wechsel von Prozessen nicht mehr richtig, kann wahrscheinlich durch weitere sleeps gelöst werden oder erledigt sich hoffentlich, wenn nicht mehr alle Punkte evaluiert werden müssen (in action process früher aufhören wenn bestimmte Anzahl Punkte evaluiert sind oder die Sicherheit für einen Punkt groß genug ist)

(defclass nn-direction-expert (generator)
  ( (number-of-neighbors :initform 2 :initarg :number-of-neighbors :accessor number-of-neighbors) ))

(defclass nn-single-evaluation-expert (evaluator) ())

(defclass nn-all-evaluation-expert (evaluator) ())

(defmethod process-run ((proc nn-direction-expert))
;  (format t "process nn-direction-expert~%")
  (cond ( (bb:chunk-queue-empty-p proc)
          (process-run proc) )
        ( T
          ; nur dummy-Funktionalität: nimmt einfach alle noch nicht besuchten Punkte aus dem TSP
          (let ( (remaining-problem (get-current-state)) )
;            (format t "process nn-direction-expert working~%")
;            (format t "other-points: ~{~a ~} / ~{~a ~} / ~{~a ~} / ~a~%"
;              (mapcar #'id (other-points remaining-problem))
;              (mapcar #'available-p (other-points remaining-problem))
;              (mapcar #'id (remove-if-not #'available-p (other-points remaining-problem)))
;              (mapcar #'id (solution (get-current-state))))
            (mapcar #'(lambda (point)
                       (bb:add-chunk-to-bb
                         (make-instance 'chunk :data (change-class point 'candidate-point))))
;                         (make-instance 'chunk :data (make-instance 'candidate-point
;                                                       :id (id point)
;                                                       :x-pos (x-pos point)
;                                                       :y-pos (y-pos point)
;                                                       :region-id (region-id point)))))
                    (remove-if-not #'available-p (other-points remaining-problem))))
          (pop-chunk-queue proc) ; wichtig um Chunk Queue zu leeren
          (process-run proc) )))


(defmethod process-run ((proc nn-single-evaluation-expert))
;  (format t "process nn-single-evaluation-expert~%")
  (cond ( (chunk-queue-empty-p proc)
          (sleep 0.001)
          (process-run proc) )
        ( T
          (let ( (next-chunk (pop-chunk-queue proc)) )
;              (format t "process nn-single-evaluation-expert working~%")
            (let ( (point-to-evaluate (data next-chunk)) )
              (cond ( (assoc :nn (evaluation-values point-to-evaluate))
;                        (when (chunk-queue-empty-p proc)
;                          (sleep 0.01))
                      (sb-thread:thread-yield)
                      (process-run proc) )
                    ( T
;                        (format t "evaluation of point ~a: dist=~a, diameter=~a, value=~a~%"
;                          (id point-to-evaluate)
;                          (point-distance (start-point (get-current-state)) point-to-evaluate)
;                          (get-problem-diameter (get-current-state))
;                          (- 1
;                             (/ (point-distance (start-point (get-current-state)) point-to-evaluate)
;                                (get-problem-diameter (get-current-state)))))
                      (push (cons :nn
                                  (- 1
                                     (/ (point-distance (start-point (get-current-state)) point-to-evaluate)
                                        (get-problem-diameter (get-current-state)))))
                            (evaluation-values point-to-evaluate))
                      (bb:chunk-modified-on-bb next-chunk)
                      (process-run proc) )))) )))




