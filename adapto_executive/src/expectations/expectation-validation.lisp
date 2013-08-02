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
  (let  ((robot-navigation-normalities (map-global-structure 'validate-expectation :robot-navigation-expectations))
         (world-physical-normalities (map-global-structure 'validate-expectation :world-physical-expectations))
         (object-physical-normalities (map-global-structure 'validate-expectation :object-physical-expectations))
         (human-duration-normalities (map-global-structure 'validate-expectation :human-duration-expectations))
         (human-activity-normalities (map-global-structure 'validate-expectation :human-activity-expectations))
         (normalities NIL))

    (setf normalities (cons
                       (average robot-navigation-normalities)
                       (cons (average world-physical-normalities)
                             (cons (average object-physical-normalities)
                                   (cons (average human-duration-normalities)
                                         (list (sum (get-rid-of-nils human-activity-normalities))))))))
    (format t "Normalities:~%")
    (format t "Robot navigation: ~s~%" (average robot-navigation-normalities))
    (format t "World physical: ~s~%" (average world-physical-normalities))
    (format t "Object physical: ~s~%" (average object-physical-normalities))
    (format t "Human durations: ~s~%" (average human-duration-normalities))
    (format t "Human activity: ~s~%" (sum (get-rid-of-NILs human-activity-normalities)))
    (format t "~% General Normality: ~s~%" (average (get-rid-of-nils normalities)))
    ))


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
    