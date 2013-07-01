(in-package :ad-exe-tests)

;; Umbrella example
(defun create-umbrella-state-observation-probabilities ()
   (let ((observation-probabilities (make-hash-table :test 'equalp)))
     (format t "Setting observation probabilities ...")
     (setf (gethash (string 'rain-umbrella) observation-probabilities) 0.9)
     (setf (gethash (string 'rain-noumbrella) observation-probabilities) 0.1)
     (setf (gethash (string 'sun-umbrella) observation-probabilities) 0.2)
     (setf (gethash (string 'sun-noumbrella) observation-probabilities) 0.8)
     observation-probabilities))

(defun create-umbrella-uniform-state-probabilities ()
   (let ((state-probabilities (make-hash-table :test 'equalp)))
     (format t "Setting uniform state probabilities ...")
     (setf (gethash (string 'DW-rain) state-probabilities) 0.5)
     (setf (gethash (string 'DW-sun) state-probabilities) 0.5)
     state-probabilities))

(defun create-umbrella-state-transition-probabilities ()
   (let ((state-transitions (make-hash-table :test 'equalp)))
     (format t "Setting state transition probabilities... ")
     (setf (gethash (string 'DW-rain-DW-rain) state-transitions) 0.7)
     (setf (gethash (string 'DW-rain-DW-sun) state-transitions) 0.3)
     (setf (gethash (string 'DW-sun-DW-sun) state-transitions) 0.7)
     (setf (gethash (string 'DW-sun-DW-rain) state-transitions) 0.3)
     state-transitions))

