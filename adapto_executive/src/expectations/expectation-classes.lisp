(in-package :ad-exe)

;; This file defines the classes of expectations that can be used

;; General expectations: Superclass of all expectations. We might add some common slots there if needed
(defclass expectation ()
  ((ready-for-validation :initarg :ready-for-validation :accessor ready-for-validation :initform NIL)))

;; Like in composite pattern, an expectation can also consist of several sub-expectations
(defclass expectations-category (expectation)
  ;; NOTE: expectations-list is only kept for backwards compatibility!!!
  ((expectations-list :initarg :expectations-list :accessor expectations-list)
   (expectations-table :initarg :expectations-table :accessor expectations-table :initform (make-hash-table))))

;; Probabilistic expectations have a discrete probability distribution PROBDIST 
(defclass probabilistic-expectation (expectation)
  ((probdist :initarg :probdist :accessor probdist)))

;; Expectations about the position of things defined by an area and and a poseStamped
(defclass position-expectation (expectation)
  ((area :initarg :area :accessor area)
   (pose :initarg :pose :accessor pose)
   (ready-for-validation :initarg :ready-for-validation :accessor ready-for-validation :initform T)))

;; Expectations if door is expected to be open or not
(defclass door-expectation (expectation)
  ((door-name :initarg :door-name :accessor door-name)
   (expected-open :initarg :expected-open :accessor expected-open)
   (is-open :initarg :is-open :accessor is-open)))

(defclass next-location-expectation (probabilistic-expectation)
  ((next-location-probdist :initarg :next-location-probdist :accessor next-location-probdist)
   (next-location :initarg :next-location :accessor next-location)))

(defclass duration-expectation (expectation)
  ((location-name :initarg :location-name :accessor location-name)
   (max-expected-duration :initarg :max-expected-duration :accessor max-expected-duration)
   (time-entered :initarg :time-entered :accessor time-entered)
   (time-left :initarg :time-left :accessor time-left)
   (ready-for-validation :initarg :ready-for-validation :accessor ready-for-validation :initform T)))

(defclass object-expectation (expectation)
  ((object :initarg :object :accessor object)
   (flexible :initarg :flexible :accessor flexible)
   (ready-for-validation :initarg :ready-for-validation :accessor ready-for-validation :initform T)))

(defclass object-on-floor-expectation (expectation)
  ((object :initarg :object :accessor object)
   (expected-on-floor :initarg :expected-on-floor :accessor expected-on-floor)))

(defclass action-expectation (expectation)
  ((action-type :initarg :action-type :accessor action-type)
   (duration :initarg :duration :accessor duration)
   (start-time :initarg :start-time :accessor start-time)
   (ready-for-validation :initarg :ready-for-validation :accessor ready-for-validation :initform T)))

(defclass navigation-action-expectation (action-expectation)
  ((action-type :initarg :action-type :accessor action-type :initform 'navigation )
   (duration :initarg :duration :accessor duration)
   (start-time :initarg :start-time :accessor start-time)
   (path-length :initarg :path-length :accessor path-length)
   (avg-speed :initarg :avg-speed :accessor :avg-speed)
   (ready-for-validation :initarg :ready-for-validation :accessor ready-for-validation :initform T)))

;; Methods for expectations categories

(defgeneric get-child-expectations (exp))

(defmethod get-child-expectations ((exp expectations-category))
  "Return child expectations of category as hashtable"
  (expectations-table exp))

(defgeneric add-or-update-expectation (expectations-category name expectation))

(defmethod add-or-update-expectation (expectations-category name expectation)
  (error "[expectations-classes.lisp] Expectations can only be added to expectations-categories!"))

(defmethod remove-expectation (a b)
  (error "[expectations-classes.lisp] Expectations can only be removed from expectations-categories!"))

(defmethod add-or-update-expectation ((exp expectations-category) name expectation)
  "Add expectation if it does not exist, otherwise update the expecatation"
  (setf (gethash name (expectations-table exp)) expectation)
  exp)

(defmethod remove-expectation ((exp expectations-category) name)
  "Remove expectation from expectations category"
  (remhash name (expectations-table exp))
  exp)

;; Validation methods of expectations MUST always return number between 0 or 1. 

(defgeneric validate-expectation (x))

(defmethod validate-expectation (x)
  (error "[expectation-classes.lisp] - No validation-function defined for this type"))

;; By default, we use average of normalities, but this is problematic when using probabilistic
;; expectations as we do for the next-location-expectations
(defmethod validate-expectation ((exp expectations-category))
  "Iterate through expectations list and return average of each validated expectation"
  (let ((normality-list NIL))
    (maphash #'(lambda (name e) 
                 (declare (ignore name))
                 (if (string= (string (type-of e)) "EXPECTATIONS-CATEGORY")
                   ;; If nested catories exist, use only first list element
                   (setf normality-list (cons (first (validate-expectation e)) normality-list))
                   (setf normality-list (cons (validate-expectation e) normality-list))))
             (expectations-table exp))
    (setf normality-list (cons (average (get-rid-of-nils normality-list)) normality-list))
    normality-list))

(defmethod validate-expectation ((exp object-on-floor-expectation))
  (let ((is-on-floor NIL))
   (when (<
          (float
           (cl-transforms:z
            (cl-transforms:origin [(pose (object exp))])))
          0.3)
     (setf is-on-floor T))
   (if (eq (expected-on-floor exp) is-on-floor)
     1
     0)))

(defmethod validate-expectation ((exp position-expectation))
  "Returns 1 if POSE of POSITION-EXPECTATION is inside AREA, else returns 0"
  (if (inside-area (area exp) (pose exp))
    1
    0))

(defmethod validate-expectation ((exp door-expectation))
  "Returns 1 if is-open = expected-open, else returns 0"
  (if (string= (string (expected-open exp)) (string (value (is-open exp)))) 
    1
    0))

(defmethod validate-expectation ((exp next-location-expectation))
  "If EXP not ready to validate, returns NIL. If EXP is ready to validaterReturns weight
   of EXP if locations match, 0 otherwise"
  (if (eq (ready-for-validation exp) NIL)
    NIL
    (progn
      (gethash (string (next-location exp)) (next-location-probdist exp)))))

(defmethod validate-expectation ((exp duration-expectation))
  "Return 1 if current time - start-time does not exceed max-expected duration,
   otherwise return 0 (TODO: maybe make transition softer by linear descent)"
  (let ((delta-t (- (get-universal-time) (time-entered exp))))
    (if (> delta-t (max-expected-duration exp))
      (if (> delta-t (* 2 (max-expected-duration exp)))
        0
        (- 1 (/
              (-
               delta-t
               (max-expected-duration exp))
              (max-expected-duration exp))))
      1)))

(defmethod validate-expectation ((exp object-expectation))
  "Returns 0 if a non-flexible object has moved, 1 otherwise"
  (cond
     ((and (has-moved (object exp)) (not (flexible exp)))
       0)
     (t 1)))

(defmethod validate-expectation ((exp navigation-action-expectation))
  "Non-generic function to check if navigation action is finished in expected time using fixed
   value for average robot speed."
  (let ((delta-t (- (roslisp:ros-time) (start-time exp))))
    ;; (format t "Navigation-action should take ~s seconds for path of length ~s ~%" (duration exp)
    ;; (path-length exp))
    ;; (format t "Navigation-action in progress since ~s seconds~%" time-since-start)
    (if (> delta-t (duration exp))
        (if (> delta-t (* 2 (duration exp)))
            0
            (- 1 (/
                  (-
                   delta-t
                   (duration exp))
                  (duration exp))))
        1)))