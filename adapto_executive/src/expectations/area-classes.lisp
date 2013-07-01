(in-package :ad-exe)

;; define area classes for validation if a pose is inside an area

;; superclass of all areas
(defclass area () ())

;; Static circles define areas around objects that are static in the world.
(defclass static-circle (area)
  ((x :initarg :x :accessor x)
   (y :initarg :y :accessor y)
   (radius :initarg :radius :accessor radius)))

;; Static tow-dimensional gaussian distributions gaussian 
(defclass static-gaussian (area)
  ((mean-x :initarg :mean-x :accessor mean-x)
   (mean-y :initarg :mean-y :accessor mean-y)
   (var-x :initarg :var-x :accessor var-x )
   (var-y :initarg :var-y :accessor var-y))
  )

;; Moving circles should be used to define areas around objects that are not static.
;; Since with moving objects x and y are subject to change, they are fluents
(defclass moving-circle (area)
  ((x :initarg :x :accessor x)
   (y :initarg :y :accessor y)
   (radius :initarg :radius :accessor radius)))

(defgeneric inside-area (area pose))

(defmethod inside-area (area pose)
  (error "[area-classes.lisp] - No inside-area function defined for this kind of area"))

(defmethod inside-area ((area static-circle) pose)
  (< (cl-transforms:v-dist (cl-transforms:make-3d-vector (tf:x (cl-transforms:origin pose))
                                                         (tf:y (cl-transforms:origin pose))
                                                         0)
                           (cl-transforms:make-3d-vector (x area) (y area) 0))
     (radius area)))

;; Return probability for pose to be in gaussian
(defmethod inside-area ((area static-gaussian) pose)
  (* (/ 1 (* 2 pi (var_x area) (var_y area)))
     (exp
      (* -1 (+
             (/  (* (- (tf:x (cl-transforms:origin [pose])) (mean_x area))
                    (- (tf:x (cl-transforms:origin [pose])) (mean_x area)) )
                 (* 2 (var_x area)))
             (/  (* (- (tf:y (cl-transforms:origin [pose])) (mean_y area))
                    (- (tf:y (cl-transforms:origin [pose])) (mean_y area)))
                 (* 2 (var_y area)))
             ))) ))

;; pose is a fluent and x,y in area are fluents
(defmethod inside-area ((area moving-circle) pose)
  (< (cl-transforms:v-dist (cl-transforms:make-3d-vector (tf:x (cl-transforms:origin [pose]))
                                                         (tf:y (cl-transforms:origin [pose]))
                                                         0)
                           (cl-transforms:make-3d-vector [(x area)] [(y area)] 0))
     (radius area)))