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

;; WARNING!!! Dirty hack for ACS Deadline! TODO: Implement normality tree traversal!
;; This will only work with depth = 2 and only categories in level 1
(defun average-normality (normality-tree)
  (float (average (mapcar #'average normality-tree))))

;; Validate all expectations in global structures
(defun validate-expectations ()
  (let ((normalities (map-global-structure 'validate-expectation :expectations)))
    normalities))

(defun start-continual-expectation-validation (seconds)
  (let ((last-validation-time 0) (avg-normality NIL))
    (setf avg-normality (average-normality (validate-expectations)))
    (format t "Avg normality: [~s] ~% Normality-tree: ~s~%"
            avg-normality
            (validate-expectations))
    ;; OUTPUT FOR ACS PAPER
    (write-average-normality "~/Desktop/avg_normality.csv" avg-normality)
    (visualize-normality avg-normality)
    
    (setf last-validation-time (roslisp:ros-time))
    (sleep seconds)
    (unless (eq (get-global-structure :expectations) NIL)
      (start-continual-expectation-validation seconds))))



    