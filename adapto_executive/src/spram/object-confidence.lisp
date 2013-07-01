(in-package :ad-exe)

;; TODO: This does not work to well due to false detections of location labels.
;; We should either return probabilities for the detection of location labels
;; or not account for the locations at all.
(defun calculate-object-confidence (plan-library objects-cache data full-spatial-model)
  "Calculates a belief distribution expressing how well the detected objects at single locations
   fit to different plans."
  (let ((most-likely-gaussian-label NIL) (object-hits 0)
        (plan-object-hits (make-hash-table)))
    ;; TODO: Get probability dist instead of only most-likely-gaussian?
    (setf most-likely-gaussian-label (label (get-most-likely-gaussian data full-spatial-model)))
    ;; Search in stprs
    (dolist (stpr (stpr-list plan-library))
      ;; check every location
      (setf object-hits 0)
      (dolist (timed-location (st-list stpr))
        (if (string= (string (location-label timed-location))  (string most-likely-gaussian-label))
          ;; Check objects at that location
          (dolist (stpr-object (object-labels timed-location))
            (dolist (observed-object objects-cache)
              (if (string= (string observed-object) (string stpr-object))
                (setf object-hits (+ 1 object-hits)))))))
      ;; (format t "Object hits for plan ~s : ~s~%" (name stpr) object-hits)
      (setf (gethash (string (name stpr)) plan-object-hits) object-hits))
    plan-object-hits))

(defun object-at-location-p (stpr observed-object)
  "Returns 1 if the OBSERVED-OBJECT appears in any location of the STPR. Else returns 0"
  (let ((object-found 0))
    (dolist (timed-location (st-list stpr))
      ;; Check objects at that location
      (dolist (stpr-object (object-labels timed-location))
        (if (string= (string observed-object) (string stpr-object))
            (progn
              ;; (format t "Found MATCH for object ~s in plan ~s~%" (string stpr-object) (name stpr))
              (setf object-found 1)))))
    object-found))

(defun calculate-object-confidence-without-locations (plan-library objects-cache data full-spatial-model)
  "Calculates a binary belief distribution over plans expressing if a detected object appears in a plan
   or not. Probability for each plan will be 1 if any object from OBJECTS-CACHE exists in plan, 0
   otherwise"
  (let ((most-likely-gaussian-label NIL)
        (object-hits 0)
        (object-found 0)
        (plan-object-hits (make-hash-table))
        (number-plans 0))
    
    (setf most-likely-gaussian-label (label (get-most-likely-gaussian data full-spatial-model)))
    (dolist (stpr (stpr-list plan-library))
      (setf number-plans (+ 1 number-plans))
      (setf object-hits 0)
      (dolist (observed-object objects-cache)
        ;; Check if object appears anywhere in stpr
        (setf object-found (object-at-location-p stpr observed-object))
        ;; Unless already 1, set hashvalues to 0 or 1
        (unless (eq (gethash (string (name stpr)) plan-object-hits) 1)
          (setf (gethash (string (name stpr)) plan-object-hits) object-found)))
      (unless (eq (gethash (string (name stpr)) plan-object-hits) 1)
        (setf (gethash (string (name stpr)) plan-object-hits) object-found)))
    plan-object-hits))

(defun weight-belief-distribution-with-variance (plan-belief)
  "Calculates the variance of the belief distribution PLAN-BELIEF and weights the beliefs
   according to it."
  (let ((sum 0) (n 0) (mean 0) (new-belief (make-hash-table)) (dev-sum 0) (dev 0))
   (maphash #'(lambda (plan prob)
                (declare (ignore plan))
                (setf sum (+ sum prob))
                (setf n (+ n 1)))
           plan-belief)
   (setf mean (/ sum n))

   (maphash #'(lambda (plan prob)
                (declare (ignore plan))
                (setf dev-sum (+ (abs (- prob mean)))))
            plan-belief)
   (setf dev (/ dev-sum n))
   
   (maphash #'(lambda (plan prob)
                (setf (gethash plan new-belief) (* prob dev)))
            plan-belief)
   (if (eq dev 0)
       new-belief
       (normalize-belief new-belief))))