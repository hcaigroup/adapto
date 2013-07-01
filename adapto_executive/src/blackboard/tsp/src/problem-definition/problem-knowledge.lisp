(in-package "TSP")

(let ( (problem-knowledge (make-hash-table)) ); hash-Table mit assoc-Liste

  (defun get-problem-knowledge (problem knowledge-item)
    (cdr (assoc knowledge-item (gethash problem problem-knowledge))))

  ; Annahme, dass ein Stück Wissen nur einmal reingeschrieben wird
  (defun add-problem-knowledge (problem knowledge-item knowledge-data &optional (assoc-list problem-knowledge))
    (let ( (problem-data (gethash problem problem-knowledge)) )
      (setf (gethash problem problem-knowledge) (cons (cons knowledge-item knowledge-data) problem-data))))

)

; Problem Bounding Box
(let ( (min-x 0)
       (max-x 0)
       (min-y 0)
       (max-y 0) )

  (defun set-problem-bounding-box (problem)
    (unless (get-problem-knowledge problem :bounding-box)
      (setf min-x (x-pos (end-point problem)))
      (setf min-y (y-pos (end-point problem)))
      ; for intermediate problems start-point is not equal to end-point!
      (dolist (pt (cons (end-point problem) (cons (start-point problem) (other-points problem))))
  ;      (when (point-available pt) -> for scaling of NN distance it is important to have all the points
          (setf min-x (min min-x (x-pos pt)))
          (setf max-x (max max-x (x-pos pt)))
          (setf min-y (min min-y (y-pos pt)))
          (setf max-y (max max-y (y-pos pt))))
      (add-problem-knowledge problem :bounding-box (list min-x max-x min-y max-y))))
)

(defun get-problem-diameter (problem)
  (set-problem-bounding-box problem)
  (destructuring-bind (problem-min-x problem-max-x problem-min-y problem-max-y)
                        (get-problem-knowledge problem :bounding-box)
    (ut:euklid-distance (list problem-min-x problem-min-y)
                        (list problem-max-x problem-max-y))))
