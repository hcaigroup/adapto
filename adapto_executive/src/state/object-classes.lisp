(in-package :ad-exe)

;; general things
(defclass thing (geometrical-form)
  ((id :initarg :id :reader id)
   (last-detection :initarg :last-detection :initform NIL :accessor last-detection)))

;; things can be checked if they have moved since their last detection

(defgeneric has-moved (thing))

;; Checks if object has moved since last detection. Returns T or NIL
;; NOTE: distance value for simulation should be lower (0.005?)
(defmethod has-moved (thing)
  "Return T if object has moved since last detection"
  (let ((has-moved-p
        (unless (null (last-detection thing))
          (> (cl-transforms:v-dist
              (cl-transforms:origin [(pose thing)])
              (cl-transforms:origin [(last-detection thing)]))
             0.2 ))))
    ;; (unless (null (last-detection thing))
    ;;   (format t "--- has_moved: distance ~s~%"
    ;;         (cl-transforms:v-dist
    ;;          (cl-transforms:origin [(pose thing)])
    ;;          (cl-transforms:origin [(last-detection thing)]))))
    has-moved-p))

;; agents
(defclass agent (thing) ())

(defclass human (agent) ())

(defclass robot (agent) ())

(defclass jido (robot jido-base-form kuka-arm-form)
  ( (arm-base-joint :initarg :arm-base-joint :accessor arm-base-joint) ))

(defclass james (robot james-base-form) ())

;; an entity is an object that is not an agent
(defclass entity (thing) ())

(defclass cube (entity cuboid) ())

(defclass door (entity)
  ( (hinge :initarg :hinge :accessor hinge)
    (door-open :initarg :door-open :accessor door-open)
    (description :initarg :description :accessor description)))
                                        
;; furniture

(defclass furniture (entity) ())

(defclass cuboard (furniture cuboid)
  ( (doors :initform () :initarg :doors :accessor doors)
    (door-joints :initform () :initarg :door-joints :accessor door-joints)))

(defclass chair (furniture chair-form) ())

(defclass table (furniture cuboid) ())


; cutlery
(defclass cutlery (entity) ())

(defclass knife (cutlery cuboid) ())

(defclass fork (cutlery spoon-like) ())

(defclass spoon (cutlery spoon-like) ())

; tableware
(defclass tableware (entity) ())

(defclass plate (tableware cylinder) ())

(defclass cup (tableware cylinder)
  ( (handle-position :initform :handle-position :accessor handle-position)))
