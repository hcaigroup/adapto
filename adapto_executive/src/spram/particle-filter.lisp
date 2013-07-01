(in-package :ad-exe)

;; A particle consists of a model-plan in form of a spatio-temporal plan description,
;; a spatial model that is created from the model-plan, observations (that are
;; represented as a list of timed-locations) and a weight 
(defclass particle ()
  ((model-stpr :initarg :model-stpr :accessor model-stpr)
   (spatial-model :initarg :spatial-model :accessor spatial-model)
   (observations :initarg :observations :accessor observations :initform NIL)
   (weight :initarg :weight :accessor weight :initform 0.5)
   (start-time :initarg :start-time :accessor start-time)))

(defgeneric add-observation-to-particle (x y duration objects particle))

;; This function adds an observation consisting of a 2D position of the human, a duration and
;; object detections at the position and recalculates the weight of the specific particle
;; Returns particle with updated weight and observations
(defmethod add-observation-to-particle (x y duration objects-cache particle)
  ;; First calculate probability for the point beeing in each gaussian of the spatial model
  (let ((most-likely-gaussian NIL) (gaussian-probability 0) (most-likely-object-string-label NIL))
    (dolist (current-gaussian (labelled-gaussians (spatial-model particle)))
      (inside-gaussian x y (gaussian current-gaussian))
      ;; (format t "Observation fits to location ~s with probability ~s~% " (label current-gaussian) (inside-gaussian x y (gaussian current-gaussian)))
      (when (> (inside-gaussian x y (gaussian current-gaussian)) gaussian-probability)
        (progn
          (setf gaussian-probability (inside-gaussian x y (gaussian current-gaussian)))
          (setf most-likely-gaussian current-gaussian)
          (setf most-likely-object-string-label (object-string-label current-gaussian)))))
    
    ;; (format t "Saw human standing for ~3$ s at ~s (with probability ~s, obsl ~s)~%" duration (string (label most-likely-gaussian)) gaussian-probability most-likely-object-string-label)
    ;; (format t "Saw human standing at ~s interacting with objects: ~s~%" (string (label most-likely-gaussian)) objects-cache)

    ;; Add observation as a timed-location to observation-stpr of particle
    (setf (observations particle) (append  (observations particle) (list (make-instance 'timed-location
            :location-label (string (label most-likely-gaussian))
            :duration duration
            :confidence gaussian-probability
            :object-labels objects-cache
            :object-string-label most-likely-object-string-label))))
    (return-from add-observation-to-particle particle)))

(defun add-observation-to-particle-set (particle-set data object-detections last-stop-time ros-time)
  (let ((new-particle-set NIL))
    ;; (format t "Adding observation: ~s,~s -- ~s ~%"
    ;;         (geometry_msgs-msg:x (geometry_msgs-msg:position (geometry_msgs-msg:pose (nav_msgs-msg:pose data))))
    ;;         (geometry_msgs-msg:y (geometry_msgs-msg:position (geometry_msgs-msg:pose (nav_msgs-msg:pose data))))
    ;;         object-detections )
    (dolist (particle particle-set)
      (setf new-particle-set (cons (add-observation-to-particle
                                    (geometry_msgs-msg:x (geometry_msgs-msg:position (geometry_msgs-msg:pose (nav_msgs-msg:pose data))))
                                    (geometry_msgs-msg:y (geometry_msgs-msg:position (geometry_msgs-msg:pose (nav_msgs-msg:pose data))))
                                    (- ros-time last-stop-time)
                                    object-detections
                                    particle)
                                   new-particle-set))       
      (setf (weight particle) (calculate-weight particle)))
    (return-from add-observation-to-particle-set new-particle-set)))


;; Calculation of particle weights
(defgeneric calculate-weight (particle))

;; Calculate particle-weight
(defmethod calculate-weight (particle)
  (let ((weight-sum 0) (observation-counter 0) (new-weight 0) (model-string "") (observation-string "") (last-observation-string NIL) (observations-confidence 0) (pattern-weight 0) (object-weight 0) (num-obj-detections 0) (timing-factor 1) (deltat 0))
    
    ;; Build string for model-pattern
    (setf model-string (create-string-from-stpr (model-stpr particle)))
    
    ;; Iterate through observations
    (dolist (timed-location (observations particle))
      
      ;; OBSERVATIONS-CONFIDENCE: Sum up confidence measures of observations based on value from gaussians
      (setf weight-sum (+ weight-sum (confidence timed-location)))
      (setf observation-counter (+ observation-counter 1))
      
      ;; PATTERN-WEIGHT: Create string from observations
      ;; Only add new location-string if different location string labels (assumption: a human never goes to the same location twice after another)  TODO: think about that!!!
      (unless (string= last-observation-string (object-string-label timed-location))
        (setf observation-string (concatenate 'string observation-string (object-string-label timed-location))))
      ;; Save last string label
      (setf last-observation-string (object-string-label timed-location))
      
      ;; OBJECT-DETECTIONS:Check every object detection (unless 0) if the object should be at the corresponding location
      (unless (eq (object-labels timed-location) NIL)
        ;; Count number of object detections
        (setf num-obj-detections (+ (length (object-labels timed-location)) num-obj-detections))
        ;; Search the location in the spatial model
        (dolist (location (labelled-gaussians (spatial-model particle)))
          (when (string= (location-label timed-location) (label location))
            ;; When correct location is found, check if object detection fits to objects in model
            (dolist (detected-object (object-labels timed-location))
              (dolist (modelled-object (objects location))
                (when (string= (string modelled-object) (string detected-object))
                  ;; and count number of object-detections that fit to the model
                  (setf object-weight (+ 1 object-weight)))))))))
    
    ;; (format t "~s of ~s object detections fit to model~%" object-weight num-obj-detections)
    (if (eq num-obj-detections 0)
        (setf object-weight 0)
        (setf object-weight (/ object-weight num-obj-detections)))
    
    ;; (format t "Comparing model with observation: ~s -> ~s Similarity: ~3$ (~s)~%" model-string observation-string (levenshtein-similarity model-string observation-string) (name (model-stpr particle)))
    (setf observations-confidence (/ weight-sum observation-counter))
    
    ;; Cut model-string to length of observation string before calculating levenshtein-similiarity
    ;; pattern-weight counts according to its model-plan-length / observations-length using a normalize gaussian function
    (if (< (length observation-string) (length model-string))
      (setf pattern-weight (*
                            (normalized-1d-gauss (length model-string) 1 (length observation-string))
                            (levenshtein-similarity (subseq model-string 0 (length observation-string)) observation-string)))
      (setf pattern-weight (*
                            (normalized-1d-gauss (length model-string) 1 (length observation-string))
                            (levenshtein-similarity model-string observation-string))))
    
    ;; Calculate timing-factor. Will be 1 as long as particle within expected time and decrease proportionally afterwards
    (setf deltat (- (roslisp:ros-time) (start-time particle)))
    (when (> deltat (duration (model-stpr particle)))
      (setf timing-factor (/ (- (* 2 (duration (model-stpr particle))) deltat) (duration (model-stpr particle)))))

    ;; Don't let timing factor get smaller than 0
    (when (< timing-factor 0)
      (setf timing-factor 0))

    ;; Use squared weight to get PF more aggressive
    ;; (setf new-weight (square (* timing-factor (avg (list observations-confidence object-weight pattern-weight)))))
    (setf new-weight (* timing-factor (avg (list observations-confidence object-weight))))
    ;; (setf new-weight (* timing-factor (avg (list (* 2 observations-confidence) pattern-weight))))

    ;; (format t "Weights for particle ~30@A$: ~3$        ( ~3$, ~3$, ~3$ -- ~1$)  (obs-conf, patterns, obj-detect -- timing-factor)~%"  (string (name (model-stpr particle))) new-weight observations-confidence pattern-weight object-weight timing-factor)
    
    (return-from calculate-weight new-weight)))

;; Take stpr, look up location strings and generate new stpr with location-strings assigned and return new stpr
(defun assign-strings-to-stpr (stpr loc-str-table)
  (let ((old-st-list (st-list stpr)) (new-st-list NIL))
    (dolist (timed-location old-st-list)
      (setf new-st-list (cons (make-instance 'timed-location
                                :location-label (location-label timed-location)
                                :duration (duration timed-location)
                                :object-labels (object-labels timed-location)
                                :confidence (confidence timed-location)
                                :object-string-label (gethash (location-label timed-location) loc-str-table)) new-st-list))
      ;; (format t "Added TL with label ~s and string ~s~%" (location-label timed-location) (gethash (location-label timed-location) loc-str-table))
      new-st-list )
    (return-from assign-strings-to-stpr (make-instance 'stpr
                                          :st-list new-st-list
                                          :plan-length (plan-length stpr)
                                          :name (name stpr)
                                          :duration (duration stpr)))))

;; generates a particle from a spatio-temporal plan description
(defun generate-particle (stpr loc-str-table)
  (make-instance 'particle :model-stpr (assign-strings-to-stpr stpr loc-str-table)
                 :spatial-model (generate-spatial-model stpr loc-str-table)
                 :observations ()
                 :start-time (roslisp:ros-time)))

;; generate n particles by uniformly picking from stpr-library
;; TODO: Initialize with first observation, Add Random Seed for better pseudo-random numbers! 
(defun initialize-uniform (n loc-str-table)
   (let ((stpr-library NIL) (particle-set NIL) (rnd NIL))
     (setf stpr-library (generate-stpr-library))
     ;; (format t "Initializing PF uniformly with ~s particles:" n)
     (dotimes (i n)
       (setf rnd (random (length (stpr-list stpr-library))))
       (setf particle-set (cons (generate-particle (nth rnd (stpr-list stpr-library)) loc-str-table) particle-set))
       ;; (format t ".")
       )
     ;; (format t "done!~%")
     (return-from initialize-uniform particle-set)))

;; generate n particles picking one of each plan from stpr-library
(defun initialize-one-of-each (loc-str-table)
   (let ((stpr-library NIL) (particle-set NIL))
     (setf stpr-library (generate-stpr-library))
     (format t "Initializing PF with one plan of each.")
     (dotimes (i (length (stpr-list stpr-library)))
       (setf particle-set (cons (generate-particle (nth i (stpr-list stpr-library)) loc-str-table) particle-set))
       (format t "."))
     (format t "done!~%")
     (return-from initialize-one-of-each particle-set)))

;; Resamples a particle set based on the weight of each particle
;; Returns the resampled particle-set
(defun resample-particle-set (particleset)
  (let ((random-particle NIL) (new-particleset NIL) (random-number NIL))
    ;; Get random particle from particleset
    (loop while (< (length new-particleset) (length particleset)) do
         (setf random-particle (nth (random (length particleset)) particleset))
         (setf random-number (random 100))
         ;; Append random particle to new particleset with probability relative to its weight
         (when (< random-number (* 100 (weight random-particle)))
           (setf new-particleset (cons (make-instance 'particle
                                         :model-stpr (model-stpr random-particle)
                                         :spatial-model (spatial-model random-particle)
                                         :observations (observations random-particle)
                                         :start-time (start-time random-particle)) new-particleset))))
    (return-from resample-particle-set new-particleset)))

;; Random particle injection to allow the particle filter to diverge once it is converged
;; n percent of the particles are initialize uniform while 100 - n percent remain from particleset
(defun inject-random-particles (n particleset loc-str-table)
  (let ((new-particleset NIL) (counter 0))
    (dolist (particle particleset)
      (when (< counter (* (/ (- 100 n) 100) (length particleset)))
        (setf new-particleset (cons particle new-particleset))
        (setf counter (+ 1 counter))))
    (setf new-particleset (append (initialize-uniform (* (length particleset) (/ n 100)) loc-str-table) new-particleset))
    ;; (format t "Injected ~s percent random particles~%" n)
    (return-from inject-random-particles new-particleset)))

;; Check if particlefilter has converged.
;; Returns NIL if not converged, name of plan with highest probability if converged
(defun converged-p (particleset plan-library)
  (let ((plans-hashtable (make-hash-table)) (best-plan-percentage 0) (best-plan NIL) (bool NIL))
    ;; Init hashtable
    (dolist (plan (stpr-list plan-library))
      (setf (gethash (string (name plan)) plans-hashtable) 0))
    ;; Fill hashtable
    (dolist (particle particleset)
      (setf (gethash (string (name (model-stpr particle))) plans-hashtable) (+ 1 (gethash (string (name (model-stpr particle))) plans-hashtable))))
    
    (dolist (plan (stpr-list plan-library))
      (setf (gethash (string (name plan)) plans-hashtable) (/ (gethash (string (name plan)) plans-hashtable)  (length particleset)) )
      ;; (format t "Plan ~s: ~3$~%" (string (name plan)) (gethash (string (name plan)) plans-hashtable) )
      (when (> (gethash (string (name plan)) plans-hashtable) best-plan-percentage)
        (setf best-plan (string (name plan)))
        (setf best-plan-percentage (gethash (string (name plan)) plans-hashtable))))
    (when (> (/ best-plan-percentage (length particleset)) 0.5)
     (format t "PF has converged to plan: ~s~%" best-plan))
    (if (> best-plan-percentage 0.5)
      (setf bool T)
      (setf bool NIL))
    (if (eq bool T)
      (return-from converged-p best-plan)
      (return-from converged-p NIL))))