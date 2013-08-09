(in-package :ad-exe)

;; This file defines the classes of expectations that can be used

;; General expectations: Superclass of all expectations. We might add some common slots there if needed
(defclass expectation ()
  ((ready-for-validation :initarg :ready-for-validation :accessor ready-for-validation :initform NIL)))

;; Like in composite pattern, an expectation can also consist of several sub-expectations
(defclass expectations-category (expectation)
  ((expectations-list :initarg :expectations-list :accessor expectations-list)))

;; Expectations about the position of things defined by an area and and a poseStamped
(defclass position-expectation (expectation)
  ((area :initarg :area :accessor area)
   (pose :initarg :pose :accessor pose)
   (ready-for-validation :initarg :ready-for-validation :accessor ready-for-validation :initform T)))

(defclass next-location-expectation (expectation)
  ((next-location-guess :initarg :next-location-guess :accessor next-location-guess)
   (next-location :initarg :next-location :accessor next-location)
   (weight :initarg :weight :accessor weight)))

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

(defclass action-expectation (expectation)
  ((action-type :initarg :action-type :accessor action-type)
   (duration :initarg :duration :accessor duration)
   (start-time :initarg :start-time :accessor start-time)
   (ready-for-validation :initarg :ready-for-validation :accessor ready-for-validation :initform T)))

(defclass navigation-action-expectation (action-expectation)
  ((action-type :initarg :action-type :accessor action-type :initform 'navigation )
   (path-length :initarg :path-length :accessor path-length)
   (avg-speed :initarg :avg-speed :accessor :avg-speed)
   (ready-for-validation :initarg :ready-for-validation :accessor ready-for-validation :initform T)))

;; Validation methods of expectations MUST always return number between 0 or 1. 

(defgeneric validate-expectation (x))

(defmethod validate-expectation (x)
  (error "[expectation-classes.lisp] - No validation-function defined for this type"))

(defmethod validate-expectation ((exp expectations-category))
  "Iterate through expectations list and return average of each validated expectation"
  (let ((normality-list NIL))
    (setf normality-list
          (loop for e in (expectations-list exp)
             collect (validate-expectation exp)))
    (average normality-list)))

(defmethod validate-expectation ((exp position-expectation))
  "Returns 1 if POSE of POSITION-EXPECTATION is inside AREA, else returns 0"
  (if (inside-area (area exp) (pose exp))
    1
    0))

(defmethod validate-expectation ((exp next-location-expectation))
  "If EXP not ready to validate, returns NIL. If EXP is ready to validaterReturns weight
   of EXP if locations match, 0 otherwise"
  (unless (eq (ready-for-validation exp) NIL)
    (if (string= (next-location-guess exp) (next-location exp))
      (weight exp)
      0)))

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
       (format t "An object moved unexpectedly...~%")
       0)
     (t 1)))

(defmethod validate-expectation ((exp navigation-action-expectation))
  "Non-generic function to check if navigation action is finished in expected time using fixed
   value for average robot speed."
  (let ((time-since-start (- (roslisp:ros-time) (start-time exp))))
    ;; (format t "Navigation-action should take ~s seconds for path of length ~s ~%" (duration exp) (path-length exp))
    ;; (format t "Navigation-action in progress since ~s seconds~%" time-since-start)
    (cond
      ;; As long as action within the expected time, return 1
      ((> (- (duration exp) time-since-start) 0) 1)
      ;; expected time has passed but not more than 1.25 the time
      ((and (< (- (duration exp) time-since-start) 0)                             ;; diff smaller 0
            (> (- (duration exp) time-since-start) (* -1 (/ (duration exp) 4))))  ;; and bigger -¼*duration
        (format t "Navigation takes a little longer, still no reason to worry...")
       0.75) ;; return 0.75
      ;; expected time has passed but not more than 1.5 the time
      ((and (< (- (duration exp) time-since-start) (* -1 (/ (duration exp) 4)) )                             ;; diff smaller 0
            (> (- (duration exp) time-since-start) (* -1 (/ (duration exp) 2))))  ;; and bigger -¼*duration
        (format t "There could be someting wrong with my navigation-plan....")
       0.5) ;; return 0.75
      ;; expected time has passed but not more than 2x the time
      ((and (< (- (duration exp) time-since-start) (* -1 (/ (duration exp) 2)) )                             ;; diff smaller 0
            (> (- (duration exp) time-since-start) (* -1 (/ (duration exp) 1))))  ;; and bigger -¼*duration
        (format t "I think there IS something wrong with my navigation")
       0.25) ;; return 0.75
      ((< (- (duration exp) time-since-start) (* -1 (duration exp)))
        (format t "Navigation alread takes 2 times longer... time to PANIC!!!!")
       0))))