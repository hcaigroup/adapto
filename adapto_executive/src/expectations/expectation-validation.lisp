(in-package :ad-exe)

;; helper function to calculate average value of a list of numbers
(defun average (args)
  (when args
    (/ (apply #'+ args) (length args))))

;; helper function to calculate the szm of a list of numbers
(defun sum (args)
  (when args
    (apply #'+ args)))

;; helper funtion gets a list of different values including NIL and
;; returns a new list with all NILs removed
(defun get-rid-of-NILs (list)
  (let ((new-list NIL))
    (dolist (item list)
      (unless (eq item NIL)
        (setf new-list (cons item new-list))))
    (reverse new-list)))

;; Validate all expectations in global structures
(defun validate-expectations ()
  (let  ((expectation-normalities (map-global-structure 'validate-expectation :expectations))
          (activity-normalities (map-global-structure 'validate-expectation :activity-expectations) ))
    (format t "Exp-normalities: ~s~%" (average expectation-normalities) )
    (format t "Activity-normalities: ~s~%" (sum (get-rid-of-NILs activity-normalities)))))

;; DISABLED due to new expectations structure
;; Continual validation of expectations every 2 seconds
;; (defun start-expectation-validation ()
;;   (let (( normalities (validate-expectations)) (average-normality 0))
;;     (setf average-normality (average normalities))
;;    (unless (null normalities)
;;      ;; (format t "Expectation-Validation: ~s ~%" normalities)
;;      (format t "Average-normality: ~s ~%" average-normality)
;;      (cond
;;        ( (< average-normality 0.1) (format t "There MUST be something wrong! (avg. normality: ~s)~%" average-normality))
;;        ( (< average-normality 0.25) (format t "I think there IS something wrong... (avg. normality: ~s)~%" average-normality))
;;        ( (< average-normality 0.75) (format t "Hmmm there could be something wrong.. (avg. normality: ~s).~%" average-normality))
;;        ( (< average-normality 0.9) (format t "Something is not as expected... (avg. normality: ~s)~%" average-normality))))
;;    (sleep 2)
;;    (start-expectation-validation)))
    