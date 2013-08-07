(in-package :ad-exe)

;; Our library contains a list of spatio-temporal plan descriptions
(defclass stpr-library ()
  ((stpr-list :initarg :stpr-list :accessor stpr-list)))

(defun generate-stpr-library ()
  (make-instance 'stpr-library
    :stpr-list (list ;; (generate-human-table-setting-plan)
                     (generate-robot-table-setting-plan)
                     (generate-unpack-shopping-plan)
                     (generate-do-the-dishes-plan)
                     (generate-pancake-plan))))

(defun generate-robot-table-setting-plan ()
  (make-instance 'stpr :st-list (list (make-instance 'timed-location
                                        :location-label 'dishwasher
                                        :duration 1.36
                                        :object-labels '(placemat))
                                      (make-instance 'timed-location
                                        :location-label 'table
                                        :duration 2.45
                                        :object-labels '(placemat))
                                      (make-instance 'timed-location
                                        :location-label 'dishwasher
                                        :duration 1.36
                                        :object-labels '(napkin))
                                      (make-instance 'timed-location
                                        :location-label 'table
                                        :duration 2.45
                                        :object-labels '(napkin))
                                      (make-instance 'timed-location
                                        :location-label 'drawer115
                                        :duration 3.4
                                        :object-labels '(plate))
                                      (make-instance 'timed-location
                                        :location-label 'table
                                        :duration 2.45
                                        :object-labels '(plate))
                                      (make-instance 'timed-location
                                        :location-label 'drawer55
                                        :duration 3.4
                                        :object-labels '(spoon))
                                      (make-instance 'timed-location
                                        :location-label 'table
                                        :duration 2.45
                                        :object-labels '(spoon))
                                      (make-instance 'timed-location
                                        :location-label 'drawer55
                                        :duration 3.4
                                        :object-labels '(knife))
                                      (make-instance 'timed-location
                                        :location-label 'table
                                        :duration 2.45
                                        :object-labels '(knife))
                                      (make-instance 'timed-location
                                        :location-label 'drawer55
                                        :duration 3.4
                                        :object-labels '(fork))
                                      (make-instance 'timed-location
                                        :location-label 'table
                                        :duration 2.45
                                        :object-labels '(fork))
                                      (make-instance 'timed-location
                                        :location-label 'drawer115
                                        :duration 3.4
                                        :object-labels '(cup))
                                      (make-instance 'timed-location
                                        :location-label 'table
                                        :duration 2.45
                                        :object-labels '(cup))
                                      )
                 :plan-length 14
                 :name 'robot-table-setting
                 :duration 55))

(defun generate-human-table-setting-plan ()
 (make-instance 'stpr :st-list (list (make-instance 'timed-location
                                        :location-label 'dishwasher
                                        :duration 2.62
                                        :object-labels '(placemat napkin))
                                     (make-instance 'timed-location
                                        :location-label 'table
                                        :duration 4.85
                                        :object-labels '(placemat napkin))
                                     (make-instance 'timed-location
                                       :location-label 'drawer115
                                       :duration 5.58
                                       :object-labels '(plate cup))
                                     (make-instance 'timed-location
                                       :location-label 'table
                                       :duration 4.85
                                       :object-labels '(plate cup))
                                     (make-instance 'timed-location
                                       :location-label 'drawer55
                                       :duration 5.58
                                       :object-labels '(spoon knife fork))
                                     (make-instance 'timed-location
                                       :location-label 'table
                                       :duration 4.85
                                       :object-labels '(spoon knife fork))
                                     )
                :plan-length 6
                :name 'human-table-setting
                :duration 30))

(defun generate-unpack-shopping-plan ()
  (make-instance 'stpr :st-list (list (make-instance 'timed-location
                                        :location-label 'counter
                                        :duration 10
                                        :object-labels '(milk))
                                      (make-instance 'timed-location
                                        :location-label 'cupboard1
                                        :duration 4.85
                                        :object-labels '(milk))
                                      (make-instance 'timed-location
                                        :location-label 'counter
                                        :duration 5.58
                                        :object-labels '(cornflakes))
                                      (make-instance 'timed-location
                                        :location-label 'cupboard2
                                        :duration 4.85
                                        :object-labels '(cornflakes))
                                      (make-instance 'timed-location
                                        :location-label 'counter
                                        :duration 5.58
                                        :object-labels '(nutella jam))
                                      (make-instance 'timed-location
                                        :location-label 'cupboard2
                                        :duration 4.85
                                        :object-labels '(nutella jam))
                                      )
                 :plan-length 6
                 :name 'unpack-shopping-plan
                 :duration 65))

(defun generate-do-the-dishes-plan ()
  (make-instance 'stpr :st-list (list (make-instance 'timed-location
                                        :location-label 'table
                                        :duration 4
                                        :object-labels '(plate cup))
                                      (make-instance 'timed-location
                                        :location-label 'sink
                                        :duration 3
                                        :object-labels '(plate cup))
                                      (make-instance 'timed-location
                                        :location-label 'table
                                        :duration 4
                                        :object-labels '(fork spoon knife))
                                      (make-instance 'timed-location
                                        :location-label 'sink
                                        :duration 4.85
                                        :object-labels '(fork spoon knife))
                                      (make-instance 'timed-location
                                        :location-label 'table
                                        :duration 5.58
                                        :object-labels '(placetmat napkin))
                                      (make-instance 'timed-location
                                        :location-label 'sink
                                        :duration 300
                                        :object-labels '(placemat fork spoon knife plate cup))
                                      )
                 :plan-length 6
                 :name 'do-the-dishes-plan
                 :duration 180))

(defun generate-pancake-plan ()
  (make-instance 'stpr :st-list (list (make-instance 'timed-location
                                        :location-label 'refrigerator
                                        :duration 4
                                        :object-labels '(pancakemix))
                                      (make-instance 'timed-location
                                        :location-label 'cooker
                                        :duration 3
                                        :object-labels '(pancakemix))
                                      (make-instance 'timed-location
                                        :location-label 'refrigerator
                                        :duration 4
                                        :object-labels '(milk))
                                      (make-instance 'timed-location
                                        :location-label 'cooker
                                        :duration 3
                                        :object-labels '(milk))
                                      (make-instance 'timed-location
                                        :location-label 'drawer55
                                        :duration 4
                                        :object-labels '(spatula))
                                      (make-instance 'timed-location
                                        :location-label 'cooker
                                        :duration 200
                                        :object-labels '(spatula milk pancake))
                                      (make-instance 'timed-location
                                        :location-label 'table
                                        :duration 5.58
                                        :object-labels '(pancake))
                                      )
                 :plan-length 7
                 :name 'make-pancake-plan
                 :duration 300))

;; This is the *MAGIC* function that translates the labels of the timed locations into the furniture objects
;; in the current kitchen using the semantic map. It should therefore query the semantic map for objects that
;; fit according to the label and create the gaussians (also using our magic knowledge base)
;; The loc-str-table contains unique string-representations for all locations generated from the stpr-library 
;; PROBLEMS here are:
;; - What happens if several storage locations are returner (kitchen has several drawers)?
;; - How to generate Gaussians for general surfaces?
;; returns a labelled gaussian
(defun get-gaussian (timed-location loc-str-table)
;; For the moment we just hardcode the gaussians for the labels we use
  (let ( (locations-kb (make-hash-table)))
    ;; IAS kitchen
    (setf (gethash 'dishwasher locations-kb) (list 'dishwasher 0.647 0.337 0.062 0.137))
    (setf (gethash 'sink locations-kb) (list 'sink 0.7 0.02 0.062 0.137))
    (setf (gethash 'table locations-kb) (list 'table -0.529 0.0429 0.077 0.098))
    (setf (gethash 'drawer55 locations-kb) (list 'drawer55 0.491 1.121 0.092 0.108))
    (setf (gethash 'drawer115 locations-kb) (list 'drawer115 -0.856 1.220 0.092 0.108))
    (setf (gethash 'counter locations-kb) (list 'counter -2.05 1.876 0.077 0.098))
    (setf (gethash 'cupboard1 locations-kb) (list 'cupboard1 0.681 1.752 0.077 0.098))
    (setf (gethash 'cupboard2 locations-kb) (list 'cupboard2 0.951 2.705 0.077 0.098))
    (setf (gethash 'refrigerator locations-kb) (list 'refrigerator 0.4 -0.726 0.092 0.108))
    (setf (gethash 'cooker locations-kb) (list 'cooker -2.125 2.628 0.077 0.098))
    
    ;; IAS kitchen, Wider gaussians (vars mal 2)
    ;; (setf (gethash 'dishwasher locations-kb) (list 'dishwasher 0.647 0.337 0.124 0.274))
    ;; (setf (gethash 'sink locations-kb) (list 'sink 0.7 0.02 0.124 0.274))
    ;; (setf (gethash 'table locations-kb) (list 'table -0.529 0.0429 0.144 0.198))
    ;; (setf (gethash 'drawer55 locations-kb) (list 'drawer55 0.491 1.121 0.194 0.216))
    ;; (setf (gethash 'drawer115 locations-kb) (list 'drawer115 -0.856 1.220 0.184 0.216))
    ;; (setf (gethash 'counter locations-kb) (list 'counter -2.05 1.876 0.144 0.186))
    ;; (setf (gethash 'cupboard1 locations-kb) (list 'cupboard1 0.681 1.752 0.144 0.186))
    ;; (setf (gethash 'cupboard2 locations-kb) (list 'cupboard2 0.951 2.705 0.144 0.186))

    ;; Garching kitchen
    ;; (setf (gethash 'drawer locations-kb) (list 'drawer 0.971 2.619 0.077 0.098))
    ;; (setf (gethash 'table locations-kb) (list 'drawer 2.175 2.643 0.092 0.099))
    ;; (setf (gethash 'stove locations-kb) (list 'stove 0.764 2.174 0.062 0.137))
    ;; (setf (gethash 'cupboard locations-kb) (list 'cupboard 0.778 3.215 0.078 0.073))
    
    
    (multiple-value-bind (label m-x m-y v-x v-y) (values-list (gethash (location-label timed-location) locations-kb))
      (when  (eq label NIL) (format t "~% WARNING: Location ~s could not be found in knowledge-base!!! No actual location information will be available!!!~%~%" (location-label timed-location)))
      ;; (format t "Adding gaussian ~s with string ~s~%" (location-label timed-location) (gethash (location-label timed-location) loc-str-table))
      (make-instance 'labelled-gaussian :label (location-label timed-location)
                     :gaussian (make-instance
                                   'static-gaussian
                                 :mean-x m-x
                                 :mean-y m-y
                                 :var-x v-x
                                 :var-y v-y)
                     :objects (object-labels timed-location)
                     :object-string-label (gethash (location-label timed-location) loc-str-table)))))