(in-package :ad-exe)

;; Our library contains a list of spatio-temporal plan descriptions
(defclass stpr-library ()
  ((stpr-list :initarg :stpr-list :accessor stpr-list)))

(defun generate-stpr-library ()
  (make-instance 'stpr-library
    :stpr-list (list 
                (generate-prepare-drink-plan)
                (generate-prepare-quark-plan)
                (generate-prepare-cornflakes-plan)
                (generate-clean-table-cornflakes-plan)
                (generate-clean-table-quark-plan)
                (generate-prepare-for-work-plan)
                ;; (generate-human-table-setting-plan)
                ;; (generate-robot-table-setting-plan)
                ;; (generate-unpack-shopping-plan)
                ;; (generate-do-the-dishes-plan)
                ;; (generate-pancake-plan)
                )))

(defun generate-prepare-drink-plan ()
  (make-instance 'stpr :st-list (list (make-instance 'timed-location
                                        :location-label 'bottle-place
                                        :duration 1
                                        :object-labels '(bottle))
                                      (make-instance 'timed-location
                                        :location-label 'table
                                        :duration 2
                                        :object-labels '(bottle))
                                      (make-instance 'timed-location
                                        :location-label 'cupboard1 
                                        :duration 4
                                        :object-labels '(glass))
                                      (make-instance 'timed-location
                                        :location-label 'table
                                        :duration 50
                                        :object-labels '(glass bottle)))
                 :plan-length 4
                 :name 'prepare-drink-plan
                 :duration 60
                 :short-name 'DW))

(defun generate-prepare-cornflakes-plan ()
  (make-instance 'stpr :st-list (list (make-instance 'timed-location
                                        :location-label 'oven
                                        :duration 1.5
                                        :object-labels '(cornflakes))
                                      (make-instance 'timed-location
                                        :location-label 'table
                                        :duration 3
                                        :object-labels '(cornflakes))
                                      (make-instance 'timed-location
                                        :location-label 'refrigerator 
                                        :duration 4
                                        :object-labels '(milk))
                                      (make-instance 'timed-location
                                        :location-label 'table
                                        :duration 2
                                        :object-labels '(milk))
                                      (make-instance 'timed-location
                                        :location-label 'cupboard0
                                        :duration 3
                                        :object-labels '(bowl))
                                      (make-instance 'timed-location
                                        :location-label 'table
                                        :duration 3
                                        :object-labels '(bowl))
                                      (make-instance 'timed-location
                                        :location-label 'drawer
                                        :duration 2
                                        :object-labels '(spoon))
                                      (make-instance 'timed-location
                                        :location-label 'table
                                        :duration 120
                                        :object-labels '(cornflakes milk spoon bowl)))
                 :plan-length 8 
                 :name 'prepare-cornflakes-plan
                 :duration 150
                 :short-name 'STC))

(defun generate-prepare-quark-plan ()
  (make-instance 'stpr :st-list (list (make-instance 'timed-location
                                        :location-label 'refrigerator
                                        :duration 3.5
                                        :object-labels '(quark))
                                      (make-instance 'timed-location
                                        :location-label 'table
                                        :duration 2
                                        :object-labels '(quark))
                                      (make-instance 'timed-location
                                        :location-label 'refrigerator 
                                        :duration 4
                                        :object-labels '(milk))
                                      (make-instance 'timed-location
                                        :location-label 'table
                                        :duration 2
                                        :object-labels '(milk))
                                      (make-instance 'timed-location
                                        :location-label 'cupboard0
                                        :duration 3
                                        :object-labels '(bowl))
                                      (make-instance 'timed-location
                                        :location-label 'table
                                        :duration 3
                                        :object-labels '(bowl))
                                      (make-instance 'timed-location
                                        :location-label 'drawer
                                        :duration 2
                                        :object-labels '(spoon))
                                      (make-instance 'timed-location
                                        :location-label 'table
                                        :duration 120
                                        :object-labels '(quark milk spoon bowl)))
                 :plan-length 8 
                 :name 'prepare-quark-plan
                 :duration 150
                 :short-name 'STQ))

(defun generate-clean-table-cornflakes-plan ()
  (make-instance 'stpr :st-list (list (make-instance 'timed-location
                                        :location-label 'table
                                        :duration 3
                                        :object-labels '(cornflakes))
                                      (make-instance 'timed-location
                                        :location-label 'oven
                                        :duration 2
                                        :object-labels '(cornflakes))
                                      (make-instance 'timed-location
                                        :location-label 'table
                                        :duration 3
                                        :object-labels '(milk))
                                      (make-instance 'timed-location
                                        :location-label 'refrigerator
                                        :duration 4.85
                                        :object-labels '(milk))
                                      (make-instance 'timed-location
                                        :location-label 'table
                                        :duration 2
                                        :object-labels '(bowl))
                                      (make-instance 'timed-location
                                        :location-label 'sink
                                        :duration 3
                                        :object-labels '(bowl))
                                      (make-instance 'timed-location
                                        :location-label 'table
                                        :duration 2
                                        :object-labels '(glass))
                                      (make-instance 'timed-location
                                        :location-label 'sink
                                        :duration 3
                                        :object-labels '(glass))
                                      (make-instance 'timed-location
                                        :location-label 'table
                                        :duration 2
                                        :object-labels '(spoon))
                                      (make-instance 'timed-location
                                        :location-label 'sink
                                        :duration 3
                                        :object-labels '(spoon)))
                 :plan-length 10
                 :name 'clean-table-cornflakes-plan
                 :duration 28
                 :short-name 'CTC))

(defun generate-clean-table-quark-plan ()
  (make-instance 'stpr :st-list (list (make-instance 'timed-location
                                        :location-label 'table
                                        :duration 3
                                        :object-labels '(quark))
                                      (make-instance 'timed-location
                                        :location-label 'refrigerator
                                        :duration 2
                                        :object-labels '(quark))
                                      (make-instance 'timed-location
                                        :location-label 'table
                                        :duration 3
                                        :object-labels '(milk))
                                      (make-instance 'timed-location
                                        :location-label 'refrigerator
                                        :duration 4.85
                                        :object-labels '(milk))
                                      (make-instance 'timed-location
                                        :location-label 'table
                                        :duration 2
                                        :object-labels '(bowl))
                                      (make-instance 'timed-location
                                        :location-label 'sink
                                        :duration 3
                                        :object-labels '(bowl))
                                      (make-instance 'timed-location
                                        :location-label 'table
                                        :duration 2
                                        :object-labels '(glass))
                                      (make-instance 'timed-location
                                        :location-label 'sink
                                        :duration 3
                                        :object-labels '(glass))
                                      (make-instance 'timed-location
                                        :location-label 'table
                                        :duration 2
                                        :object-labels '(spoon))
                                      (make-instance 'timed-location
                                        :location-label 'sink
                                        :duration 3
                                        :object-labels '(spoon)))
                 :plan-length 10
                 :name 'clean-table-quark-plan
                 :duration 28
                 :short-name 'CTQ))

(defun generate-prepare-for-work-plan ()
  (make-instance 'stpr :st-list (list (make-instance 'timed-location
                                        :location-label 'table
                                        :duration 2
                                        :object-labels '(bottle))
                                      (make-instance 'timed-location
                                        :location-label 'table2
                                        :duration 8
                                        :object-labels '(bottle backpack bagpack suitcase))
                                      (make-instance 'timed-location
                                        :location-label 'door 
                                        :duration 4
                                        :object-labels '(backpack bagpack suitcase)))
                 :plan-length 3
                 :name 'prepare-for-work-plan
                 :duration 18
                 :short-name 'PW))

;; This is the *MAGIC* function that translates the labels of the timed locations into the
;; furniture objects in the current kitchen using the semantic map. It should therefore query
;; the semantic map for objects that fit according to the label and create the gaussians (also
;; using our magic knowledge base)
;; The loc-str-table contains unique string-representations for all locations generated from
;; the stpr-library 
;; PROBLEMS here are:
;; - What happens if several storage locations are returned (kitchen has several drawers)?
;; - How to generate Gaussians for general surfaces?
;; returns a labelled gaussian
(defun get-gaussian (timed-location loc-str-table)
;; For the moment we just hardcode the gaussians for the labels we use
  (let ( (locations-kb (make-hash-table)))
    ;; IAS kitchen

    ;; Real data
    (setf (gethash 'drawer locations-kb) (list 'drawer 1.1 2.659 0.077 0.098))               ;; cuttlery
    (setf (gethash 'table locations-kb) (list 'table 1.75 3.15 0.184 0.169))                 ;; eating place
    (setf (gethash 'table2 locations-kb) (list 'table2 1.6 1.6 0.169 0.092))                 ;; backpack
    (setf (gethash 'oven locations-kb) (list 'oven 0.764 2.174 0.062 0.169))                 ;; cornflakes
    (setf (gethash 'cupboard1 locations-kb) (list 'cupboard1 0.95 3.015 0.078 0.073))        ;; glass
    ;;(setf (gethash 'cupboard5 locations-kb) (list 'cupboard5 1.342 1.487 0.078 0.073))     ;; 
    ;;(setf (gethash 'cupboard2 locations-kb) (list 'cupboard2 1.09 2.657 0.078 0.073))      ;;
    (setf (gethash 'refrigerator locations-kb) (list 'refrigerator 0.9 1.75 0.078 0.073))    ;; milk/quark/butter/cheese
    (setf (gethash 'cupboard0 locations-kb) (list 'cupboard0 0.95 3.54 0.078 0.073))         ;; plate/bowl
    (setf (gethash 'sink locations-kb) (list 'sink 0.75 3.11 0.062 0.137))                   ;; sink means the place above the dishwasher where dishes are put when cleaning (not really the sink)
    (setf (gethash 'bottle-place locations-kb) (list 'bottle-place 0.9 2.991 0.062 0.169))   ;; bottle 
    (setf (gethash 'door locations-kb) (list 'door 3.3 3.3 0.177 0.198))                     ;; door (approximate)

    ;; currently not used
    (setf (gethash 'cooker locations-kb) (list 'cooker 2.175 1.443 0.092 0.099))
    
    (multiple-value-bind (label m-x m-y v-x v-y)
        (values-list (gethash (location-label timed-location) locations-kb))
      (when  (eq label NIL)
        (format t "~% WARNING: Location ~s could not be found in knowledge-base!!! No actual location information will be available!!!~%~%" (location-label timed-location)))
      ;; (format t "Adding gaussian ~s with string ~s~%" (location-label timed-location) (gethash (location-label timed-location) loc-str-table))
      (make-instance 'labelled-gaussian :label (location-label timed-location)
                     :gaussian (make-instance
                                   'static-gaussian
                                 :mean-x m-x
                                 :mean-y m-y
                                 :var-x v-x
                                 :var-y v-y)
                     :objects (object-labels timed-location)
                     :object-string-label (gethash (location-label timed-location)
                                                   loc-str-table)))))