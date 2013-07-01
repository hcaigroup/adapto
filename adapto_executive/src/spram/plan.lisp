(in-package :ad-exe)

;; This file defines the classes of plan. A plan contains a spatio-temporal plan representation,
;; a spatial model and the observations of locations (from the spatial model) (stpo) so far

(defclass plan ()
  ((stpr :initarg :stpr :accessor stpr)
   (spatial-model :initarg :spatial-model :accessor spatial-model)
   (stpo :initarg :stpo :accessor stpo)))

;; A timed location includes the location (according to the spatial model) and a duration
;; Also an optional confidence value is provided which is set to 0 by default
;; Object string label is a label that defines a unique place for string comparisons of
;; plan patterns
;; TODO: Now obsĺ come from knowledge-base but should be generated automatically
;; (check if place is unique => new label, instead use old label)
(defclass timed-location ()
  ((location-label :initarg :location-label :accessor location-label)
   (duration :initarg :duration :accessor duration)
   (object-labels :initarg :object-labels :accessor object-labels :initform ())
   (confidence :initarg :confidence :accessor confidence :initform 0)
   (object-string-label :initarg :object-string-label :accessor object-string-label :initform NIL)))

;; A spatio-temporal plan representation is a list of timed-locations with name and
;; duration in seconds
(defclass stpr ()
  ((st-list :initarg :st-list :accessor st-list)
   (plan-length :initarg :plan-length :accessor plan-length)
   (name :initarg :name :accessor name)
   (duration :initarg :duration :accessor duration)
   (short-name :initarg :short-name :accessor short-name)))

(defclass labelled-gaussian ()
  ((label :initarg :label :accessor label)
   (gaussian :initarg :gaussian :accessor gaussian)
   (objects :initarg :objects :initform () :accessor objects)
   (times-visited :initarg :times-visited :accessor times-visited :initform 1)
   (object-string-label :initarg :object-string-label :accessor object-string-label)))

;; A spatial model is a list of two-dimensional probability distributions and a label
;; that represent the locations
(defclass spatial-model ()
  ((labelled-gaussians :initform ()
                       :initarg
                       :labelled-gaussians
                       :accessor labelled-gaussians)))

(defgeneric generate-spatial-model (stpr loc-str-table))

;; At the moment we use the gaussians defined in our kitchen-knowledge implementation to generate
;; the spatial model. TODO: Search for a more generic solution
(defmethod generate-spatial-model (stpr loc-str-table)
  "Generates a spatial-model from the spatio-temporal plan respresentation STPR. LOC-STR-TABLE is
   a lookup-table that assigns a unique string id to each location. The generated spatial model
   also keeps track of objects defined in the STPR and assigns them to the locations."
  (let ((spatial-model ()) (location-labels ()))
    (setq spatial-model (make-instance 'spatial-model :labelled-gaussians ()))
    (dolist (current-location (st-list stpr))
      (cond
        ((member (location-label current-location) location-labels) 
          ;; (format t "Location ~s already known. Now updating object information~%"
          ;;         (location-label current-location))
         (dolist (current-labelled-gaussian (labelled-gaussians spatial-model))
           (when (string= (location-label current-location) (label current-labelled-gaussian))
             (setf (times-visited current-labelled-gaussian)
                   (+ 1 (times-visited current-labelled-gaussian)))
             ;; (format t  "Location ~s already known. Updating counter to ~s~%"
             ;;         (location-label current-location) (times-visited current-labelled-gaussian))
             (dolist (current-object-label (object-labels current-location))
               (cond
                 ((member current-object-label (objects current-labelled-gaussian)))
                 (t
                   ;; (format t "Adding object ~s to location ~s~%"
                   ;;         current-object-label (location-label current-location))
                   (setf (objects current-labelled-gaussian)
                         (append (list current-object-label)
                                 (objects current-labelled-gaussian)))))))))
            (t
              ;; (Format t "Adding new location ~s with object ~s to spatial model...~%"
              ;;         (location-label current-location) (object-labels current-location) )
              (setf (labelled-gaussians spatial-model)
                    (cons (get-gaussian current-location loc-str-table)
                          (labelled-gaussians spatial-model))) 
              (setf location-labels
                    (append location-labels (list (location-label current-location)))))))
    ;; (format t "Created spatial model with the following locations: ~s~%" location-labels)
    (return-from generate-spatial-model spatial-model)))

(defun merge-spatial-model (spatial-model-1 spatial-model-2)
  "This function creates a mergede spatial model containing all labelled gaussians of
   SPATIAL-MODEL-1 and SPATIAL-MODEL-2"
  (let ((spatial-labels ()))
    (dolist (labelled-gauss-1 (labelled-gaussians spatial-model-1))
      (setf spatial-labels (cons (label labelled-gauss-1) spatial-labels)))
    (dolist (labelled-gauss-2 (labelled-gaussians spatial-model-2))
      (unless (member (label labelled-gauss-2) spatial-labels)
        ;; (format t "Adding label ~s~%" (label labelled-gauss-2))
        (setf spatial-labels (cons (label labelled-gauss-2) spatial-labels))
        (setf (labelled-gaussians spatial-model-1)
              (cons labelled-gauss-2 (labelled-gaussians spatial-model-1)))))
    ;; (format t "############### Length merged spatial-model: ~s~%"
    ;;         (length (labelled-gaussians spatial-model-1)))
    (return-from merge-spatial-model spatial-model-1)))

(defun create-full-spatial-model (stpr-library loc-str-table)
  "Creates a full spatal model containing all locations of all stprs in STPR-LIBRARY taking
   into account the unique string labes from LOC-STR-TABLE"
  (let ((full-spatial-model (make-instance 'spatial-model :labelled-gaussians ())))
    (dolist (stpr (stpr-list stpr-library))
      (format t "Procesing stpr: ~s~%" (name stpr))
      (setf full-spatial-model
            (merge-spatial-model full-spatial-model
                                 (generate-spatial-model stpr loc-str-table))))
    (return-from create-full-spatial-model full-spatial-model)))

;; TODO: Could return probability distribution over all likely locations instead of only one.
(defun get-most-likely-gaussian-point (x y spatial-model)
  "Queries the SPATIAL-MODEL to which location the MOTION-TRACKING-DATA most likely corresponds"
  (let ((most-likely-gaussian NIL) (gaussian-probability 0) (most-likely-object-string-label NIL))
    
    (dolist (current-gaussian (labelled-gaussians spatial-model))
      (inside-gaussian x y (gaussian current-gaussian))
      ;; (format t "Observation fits to location ~s with probability ~s~% "
      ;;         (label current-gaussian)
      ;;         (inside-gaussian x y (gaussian current-gaussian)))
      (when (> (inside-gaussian x y (gaussian current-gaussian)) gaussian-probability)
        (progn
          (setf gaussian-probability (inside-gaussian x y (gaussian current-gaussian)))
          (setf most-likely-gaussian current-gaussian)
          (setf most-likely-object-string-label (object-string-label current-gaussian)))))
    most-likely-gaussian))

   ;; TODO: Could return probability distribution over all likely locations instead of only one.
(defun get-most-likely-gaussian (motion-tracking-data spatial-model)
  "Queries the SPATIAL-MODEL to which location the MOTION-TRACKING-DATA most likely corresponds"
  (let ((x 0) (y 0))
    (setf x (geometry_msgs-msg:x
             (geometry_msgs-msg:position
              (geometry_msgs-msg:pose (nav_msgs-msg:pose motion-tracking-data)))))
    (setf y (geometry_msgs-msg:y
             (geometry_msgs-msg:position
              (geometry_msgs-msg:pose (nav_msgs-msg:pose motion-tracking-data)))))
    (get-most-likely-gaussian-point x y spatial-model)))