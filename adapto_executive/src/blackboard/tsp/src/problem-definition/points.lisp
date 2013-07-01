(in-package "TSP")

; ---------------------------------------------;
; -- DEFINNITION OF POINTS --------------------;
; ---------------------------------------------;

(defclass point ()
  ( (id :initform 0 :initarg :id :accessor id)
    (x-pos :initform 0 :initarg :x-pos :accessor x-pos)
    (y-pos :initform 0 :initarg :y-pos :accessor y-pos)
    (available-p :initform T :accessor available-p)
    (region-id :initform 0 :initarg :region-id :accessor region-id) ))
;    (neighbors :initform () :initarg :neighbors :accessor neighbors) ))


; ---------------------------------------------;
; -- GENERAL OPERATIONS ON POINTS -------------;
; ---------------------------------------------;

(defgeneric point-distance (p0 p1))

(defmethod point-distance ((p0 point) (p1 point))
  (ut:euklid-distance (list (x-pos p0) (y-pos p0))
                      (list (x-pos p1) (y-pos p1))))

(defun evaluate-path (path)
  (apply #'+ (mapcar #'point-distance path (rest path))))
