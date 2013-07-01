(in-package "BB")

(defclass chunk ()
  ( (data :initarg :data :initform () :accessor data)
    (still-exists-p :initform T :reader still-exists-p) ))

(defgeneric forget-chunk (chunk))

(defmethod forget-chunk ((ch chunk))
  (setf (slot-value ch 'still-exists-p) nil))

(defmethod reanimate-chunk ((ch chunk))
  (setf (slot-value ch 'still-exists-p) T))

; evtl. könnten chunks auch assoziationen (d.h. pointer) zueinander haben