(in-package :ad-exe)

(defun monitor-best-particle (particle-set plan-name ros-topic prob reset-when-ending-detected)
  "Gets the particle with the highest weight and check which places have already been visited.
   Also returns particle set, which gets reset as soon as a finished plan is recognized "
  (let ((best-particle NIL) (best-similarity 0) (model-string "") (observation-string "")
        (observed-location-counter 0) (plan-finished T) (previous-location-label "")
        (locations-points NIL) (header NIL))
    ;; Get particle with the most similar observations (according to levenshtein distance)
    (dolist (particle particle-set)
      ;; Check the plans to which PF has converged
      (when (string= (name (model-stpr particle)) plan-name)
        (setf model-string (create-string-from-stpr (model-stpr particle)))
        (setf observation-string (create-string-without-doubles (observations particle)))
        (when (> (levenshtein-similarity model-string observation-string) best-similarity)
          ;; (format t "found plan ~s with similarity of ~s to be best guess. model->obs: ~s ---- ~s (WEIGHT: ~s) ~%"
          ;;         (name (model-stpr particle)) (levenshtein-similarity model-string observation-string )
          ;;         model-string observation-string (weight particle))
          (setf best-similarity (levenshtein-similarity model-string observation-string))
          (setf best-particle particle)))
      ;; reset strings for every particle
      (setf model-string "")
      (setf observation-string ""))
     
    (unless (eq best-particle NIL)
      (format t "~%~%****** James: I think you are executing the plan: ~s *******~%~%"
              (name (model-stpr best-particle))))

    (setf plan-finished T)
    ;; Check which places already have been visited and which not
    (dolist (labelled-gaussian (labelled-gaussians (spatial-model best-particle)))
      (dolist (timed-location (observations best-particle))
        (when (string= (location-label timed-location) (string (label labelled-gaussian)))
          ;; NOTE: Here we assume that a human does not go away from a location just to come back
          ;; to the same location with visiting another place in between!!!
          ;; This assumption will have to vanish if we account for partial human tracking...
          (unless (string= (location-label timed-location) previous-location-label)
           (setf observed-location-counter (+ 1 observed-location-counter))))
        (setf previous-location-label (location-label timed-location)))
      (unless (< (- (times-visited labelled-gaussian) observed-location-counter) 1)
        (format t "Human will go to location ~s ~s more times~%"
                (label labelled-gaussian) (- (times-visited labelled-gaussian)
                                             observed-location-counter) )
        ;; Collect gaussians as list of 3d-vectors for visualization
        (setf locations-points
              (append (gaussian->points (gaussian labelled-gaussian) prob) locations-points))
        (setf plan-finished NIL))
      (setf observed-location-counter 0))
    (setf header (roslisp:make-msg "std_msgs/Header"
                                   (stamp) (roslisp:ros-time)
                                   (frame_id) "map"))
    ;; (unless (eq locations-points NIL))
     (let ((cmap (points->collision-map locations-points))
            (loc-pub (roslisp:advertise ros-topic "arm_navigation_msgs/CollisionMap")) )
       (roslisp:publish loc-pub cmap))
    
    (when reset-when-ending-detected
     (when plan-finished
      (format t "~%~%******** James: I think you just finished the plan: ~s *******~%~%"
              (name (model-stpr best-particle)) )
      (setf particle-set
            (inject-random-particles 100 particle-set
                                     (generate-location-string-table (generate-stpr-library))))))
    (return-from monitor-best-particle particle-set)))

(defun init-plan-good-observations-table (plan-library)
  "Creates a hashtable with the name of every plan in PLAN-LIBRARY and an empty list"
  (let ((plan-good-observations-table (make-hash-table :test 'equalp)))
    (format t "Initializing Plan-Good-Observations-Table...~%")
    (dolist (stpr (stpr-list plan-library))
      (format t "---- Processing Plan: ~s~%" (name stpr))
      (setf (gethash (name stpr) plan-good-observations-table) NIL))
    (format t "...DONE.~%")
    (return-from init-plan-good-observations-table plan-good-observations-table)))

(defun get-good-observations (old-table plan-probabilities threshhold motion-tracking-data
                              last-motion-data loc-str-table plan-library)
  "This function creates the table NEW-TABLE that includes a plan and observations from
   MOTION-TRACKING-DATA that have been made while the probability of a plan
   (stored in PLAN-PROBABILITIES) was greater than a THRESHHOLD. For the assignment
   of the locations, a LOCAL-SPATIAL-MODEL is generated using the PLAN-LIBRARY
   and the LOC-STR-TABLE"
  (let ((new-table (make-hash-table :test 'equalp)) (local-spatial-model NIL)
        (local-observation NIL) (last-observation NIL) (last-local-observation NIL))
    (maphash #'(lambda (plan observations)
                 ;; (maphash #'print-hash-entry plan-probabilities)
                 (if (< (gethash (string plan) plan-probabilities) threshhold)
                     (setf (gethash plan new-table) NIL)
                     (progn
                       ;; (format t "Observation is GOOD, adding point ~s/~s...~%~%" x y)
                       ;; Find stpr for plan
                       (dolist (stpr (stpr-list plan-library))
                         (when (equalp (string (name stpr)) (string plan))
                           (setf local-spatial-model (generate-spatial-model stpr loc-str-table))
                           (setf local-observation
                                 (string (label (get-most-likely-gaussian
                                                 motion-tracking-data
                                                 local-spatial-model))))
                           (unless (eq last-motion-data NIL)
                             (setf last-local-observation
                                   (string (label (get-most-likely-gaussian
                                                   last-motion-data
                                                   local-spatial-model)))))
                           (setf last-observation
                                 (string (first observations)))
                           ;; If first observation detected, also add last one
                           (unless (eq last-motion-data NIL)
                             (when (eq (gethash plan old-table) NIL)
                               (setf observations (cons last-local-observation observations))))
                           (if (equalp local-observation last-observation)
                               ;; Only add observation if different from the last one
                               (setf (gethash plan new-table) observations)
                               (setf (gethash plan new-table)
                                     (cons
                                      (string (label (get-most-likely-gaussian
                                                      motion-tracking-data
                                                      local-spatial-model)))
                                      observations)))))))) old-table)
    (return-from get-good-observations new-table)))


(defun predict-locations (good-observations plan-library plan-probabilities loc-str-table)
  "This function compares the GOOD-OBSERVATIONS with the locations of the plans in PLAN-LIBRARY
   by just checking which locations have already been visited and which not and weighting them
   with the corresponding PLAN-PROBABILITIES."
  (let ((plan-finished NIL) (local-spatial-model NIL) (observed-location-counter 0)
        (locations-points NIL) (prob NIL) (header NIL) (ros-topic NIL) (best-plan-prob 0)
        (best-plan NIL) (number-plan-location 0) (number-visited-plan-locations 0)
        (plan-percentage 0) (times-to-visit 0) (production-state-name NIL)
        (monitoring-belief (make-hash-table :test 'equalp)))
    
    ;; Get plan with highest probability
    (maphash #'(lambda (plan prob)
                 (when (> prob best-plan-prob)
                   (setf best-plan-prob prob)
                   (setf best-plan (string plan))))
             plan-probabilities)
    
    ;; Keep track if plan has been finished according to visited locations
    (with-open-file (file "~/Desktop/monitoring.csv"
                            :direction :output
                            :if-exists :append
                            :if-does-not-exist :create)
      (format file "~s," (roslisp:ros-time))
      (dolist (stpr (stpr-list plan-library))
         (setf prob (gethash (string (name stpr)) plan-probabilities))
        ;; (format t "########### Monitoring plan ~s (Probability: ~s)~%:"
        ;;          (string (name stpr)) (gethash (string (name stpr)) plan-probabilities))
        (setf local-spatial-model (generate-spatial-model stpr loc-str-table))
        (setf plan-finished T)
        
        ;; Check which places already have been visited and which not
        (format file "[")
        (dolist (labelled-gaussian (labelled-gaussians local-spatial-model))
          ;; Count number of good observations for every location
          (unless (eq good-observations NIL)
            (dolist (observation (gethash (name stpr) good-observations))
              (when (string= (string observation) (string (label labelled-gaussian)))
                (setf observed-location-counter (+ 1 observed-location-counter)))))

          (setf times-to-visit (-  (times-visited labelled-gaussian)
                                   observed-location-counter))
          
          ;; Create binary belief state with full location names 
          (setf production-state-name (concatenate 'string
                                                   (string (get-short-plan-name (name stpr)))
                                                   "-"
                                                   (string (label labelled-gaussian))))
          
          (if (< times-to-visit 1)
             (progn
               ;; (format t "loc-prob: [~s : 0]~%" production-state-name)
               (setf (gethash production-state-name monitoring-belief) 0))
             (progn
               ;; (format t "loc-prob: [~s : 1]~%" production-state-name)
               (setf (gethash production-state-name monitoring-belief) 1)))
          
          
          ;; (format t "[~s] Visits/Count: ~s / ~s [left: ~s] done: ~s~%"
          ;;         (string (label labelled-gaussian))
          ;;         observed-location-counter
          ;;         (times-visited labelled-gaussian) times-to-visit
          ;;         (< times-to-visit 1))
 
            
          ;; Calculate percentage
          (setf number-plan-location (+ (times-visited labelled-gaussian)
                                        number-plan-location))
          (setf number-visited-plan-locations (+ observed-location-counter
                                                 number-visited-plan-locations))
          (setf observed-location-counter 0)
          ;; Check if all places have been visited as often as expected
          (unless (< (- (times-visited labelled-gaussian) observed-location-counter) 1)
            (format file "~s:~s," (string (label labelled-gaussian))
                    (- (times-visited labelled-gaussian) observed-location-counter))

            ;; Collect gaussians as list of 3d-vectors for visualization
            (setf locations-points (append (gaussian->points
                                            (gaussian labelled-gaussian) prob)
                                           locations-points))
            (setf plan-finished NIL)))
        
        (unless (eq number-plan-location 0)
          (setf plan-percentage (/ number-visited-plan-locations number-plan-location)))
        (setf number-visited-plan-locations 0)
        (setf number-plan-location 0)
        ;; (if (eq prob best-plan-prob)
        ;;   (format t "=> ")
        ;;   (format t "   "))
        ;; (format t "[~4s] Plan-state: ~30s : ~4f ~%" (* 100 prob) (name stpr) plan-percentage)
        ;; (when plan-finished
        ;;    (format t " ------- POSSIBLE PLAN ENDING DETECTED: ~s (prob: ~s)--------~%" (string (name stpr)) prob)
        ;;    )
        (setf observed-location-counter 0)
        ;; (format file "PROB:~s]," prob)
      ;; publish stored locations for visualization
      (setf header (roslisp:make-msg "std_msgs/Header"
                                     (stamp) (roslisp:ros-time)
                                     (frame_id) "map"))
      (setf ros-topic (replace-all (string (name stpr)) "-" "_"))
      (let ((cmap (points->collision-map locations-points))
             (loc-pub (roslisp:advertise ros-topic "arm_navigation_msgs/CollisionMap")))
        (roslisp:publish loc-pub cmap))
      ;; If current-plan is best guess, publish on "best-plan" topic
      (when (string= (string (name stpr)) best-plan)
        (let ((cmap (points->collision-map locations-points))
               (loc-pub (roslisp:advertise "best_plan" "arm_navigation_msgs/CollisionMap")))
          (roslisp:publish loc-pub cmap))))
      (format file "~%"))
    monitoring-belief))