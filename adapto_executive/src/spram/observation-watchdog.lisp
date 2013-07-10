(in-package :ad-exe)

;;; This method is the actual entry-point that starts the observation watchdog
(let ((sub1 NIL) (sub2 NIL) (last-pose NIL) (v-dist NIL) (last-stop-time NIL) (last-human-reading 0)
      (deltaT 0) (loc-str-table NIL) (current-object-detections-string NIL)
      (objects-cache NIL) (last-output-time 0) (human-x) (human-y 0) (stpr-library NIL)
      (full-spatial-model NIL) (hmm (make-instance 'hmm)) (plan-probs NIL) ;; (merged-plan-probs NIL)
      (plan-library (generate-stpr-library)) (good-plan-observations NIL) (last-stop-pose NIL)
      (last-motion-data NIL) (last-location-observation NIL) (last-duration 0)
      ;; (plan-object-hits NIL)
      ;; (last-random-injection-time 0)
      ;;(walking-dir-publisher (roslisp:advertise "walking_dir" "geometry_msgs/Pose"))
      (walking-dir-publisher NIL)
      (monitoring-belief NIL) (merged-belief NIL)
      )
    ;; Starts watchdog that checks if human is standing still
  (defun start-observation-watchdog ()
    "Initialize SPRAM module by generating a spatial model, unique string labels and init the hmm"
    (setf stpr-library plan-library)
    (setf loc-str-table (generate-location-string-table stpr-library))
    (setf full-spatial-model (create-full-spatial-model stpr-library loc-str-table))
    
    (format t "~%~% --------------------------- HMM INIT START--------------------------------~%")
    (setf (belief hmm) (create-uniform-state-probabilities-from-stpr-library plan-library))
    (setf (start-belief hmm) (create-uniform-state-probabilities-from-stpr-library plan-library))
    (setf (state-transitions hmm) (create-state-transition-probabilities))

    ;; NOTE!!! Here we have to decide if we want to use the estimated emission probabilities
    ;; or the ones calculated with ML
    ;; (setf (observation-probabilities hmm) (estimate-emission-probs-probdist))
    (setf (observation-probabilities hmm) (create-state-observation-probabilities))

    (format t "~%~% --------------------------- HMM INIT DONE --------------------------------~%")

    (setf good-plan-observations (init-plan-good-observations-table plan-library))
    (let (;; (params (get-real-params))
          (params (get-morse-params)))
      
      (format t "~%~% --------------------------- OBSERVATION PARAMS: --------------------------~%")
      (format t "| movent distance: ~15s                                          |~%" (movement-distance params))
      (format t "| objects ROStopis: ~15s                                         |~%" (objects-ros-topic params))
      (format t "| human-movement-time: ~15s~%" (human-movement-time params))
      (format t "--------------------------------------------------------------------------~%")
      
      (roslisp:with-ros-node ("observation-watchdog" :spin t)
      (setf sub1
            (roslisp:subscribe "/Human/Pose"
                               "nav_msgs/Odometry"
                               #'observe-human-motion
                               :max-queue-length 1))
      (setf sub2
            (roslisp:subscribe (objects-ros-topic params)
                               "std_msgs/String"
                               #'update-object-detections
                               :max-queue-length 1))
      (setf walking-dir-publisher
            (roslisp:advertise "walking_dir" "nav_msgs/Odometry"))
      (start-statevar-update))))

  (defun stop-observation-watchdog ()
    "Unsubscribe from ROS topics and terminate connection to ROS."
     (roslisp:unsubscribe sub1)
     (roslisp:unsubscribe sub2)
     (shutdown-ros)
     (format t "STOPPED OBSERVATION WATCHDOG"))
  
  (defun update-object-detections (object-tracker-data)
    "Read from OBJECT-TRACKER-DATA ROS topic and update object state-variables if human was
     in reach of an object and the object has moved since last detection."
    ;; package has to be set, otherwise it will be CL-USER and statevars will not bet found.
    (let ((*package* (find-package :ad-exe)))
    (setf current-object-detections-string
          (read-from-string (std_msgs-msg:data object-tracker-data)))
    
    ;; Object cache keeps track of objects that were detected, in reach of the human and have moved
    (dolist (object current-object-detections-string)
      (destructuring-bind (object-name type obj-x obj-y &rest other-args) object
        (declare (ignore type))
        (declare (ignore other-args))
        
        (when (in-reach human-x human-y obj-x obj-y)
          ;; (format t "OBJ ~s is in reach ~%" object-name)
          (when (isgv :kitchen-object object-name)
            (when (has-moved (value (getgv :kitchen-object object-name)))
              ;; (format t "--- OBJECT DETECTED --- Object ~s was in reach of human and
              ;;            has moved since last detection --- OBJECT DETECTED --- !!!!!!!~%"
              ;;         object-name)
              (unless  (member object-name objects-cache)
                (setf objects-cache (cons object-name objects-cache))))))))))
  
  ;; Parameters. Times are in seconds, distance is in meters
  (let ((output-time 0.5) 
        (params (get-real-params))
        (motion-data NIL)
        (last-orientation 0)
        (walking-direction 0)
        (last-walking-direction 0)
        (last-data NIL))
    ;; Callback-function of watchdog
    (defun observe-human-motion (data)
      (setf human-x (get-2d-x data))
      (setf human-y (get-2d-y data))
      (unless (eq last-pose NIL)
        (setf deltaT (- (std_msgs-msg:stamp (nav_msgs-msg:header data)) last-human-reading))
        
        ;; Visualization cycle. Every OUTPUT-TIME seconds plan probabilities are output
        (when (> (- (roslisp:ros-time) last-output-time) output-time)
           (setf last-output-time (roslisp:ros-time))
           (setf plan-probs (calculate-plan-probabilities (belief hmm)))
           (unless (eq plan-probs NIL) (visualize-plan-probs plan-probs))
           (unless (eq plan-probs NIL)
             (write-plan-probs-to-csv plan-probs "~/Desktop/plan-probs.csv"))
           (unless (eq merged-belief NIL)
             (write-loc-probs-to-csv
              merged-belief "~/Desktop/loc-probs.csv")))
        
        ;; Check if human has moved every movement-time seconds
        (when (> deltaT (human-movement-time params))
          (setf last-human-reading (std_msgs-msg:stamp (nav_msgs-msg:header data)))

          (setf v-dist (distance data last-pose))
          (setf last-stop-pose data)
          (if (< v-dist (movement-distance params))
              (progn
                ;; Human just stopped => save timestamp and motion-data
                (when (eq last-stop-time NIL)
                  (setf last-stop-time (roslisp:ros-time))
                  (setf last-motion-data motion-data)
                  (setf motion-data data)))
              (progn
                (setf walking-direction (get-walking-direction-yaw data last-data))
                (post-walking-direction walking-dir-publisher data walking-direction)
                ;; Human starts moving again => calculate time and add an observation
                (unless (eq last-stop-time NIL)
                  ;; ############################################################################
                  ;; Here we put the heuristics to improve location detections...
                  ;; TODO: Generalize and seperate functionality from MAIN  (after RSS Deadline)
                  ;; ############################################################################

                  ;; Calculate distance between last location observations
                  (let ((loc-obs-dist 1))
                    (unless (eq last-location-observation NIL)
                      (setf loc-obs-dist (distance motion-data last-location-observation))
                      ;; (format t "[loc-dist: ~2,5f]  " loc-obs-dist)
                      )
                    (setf last-location-observation
                          (geometry_msgs-msg:position (geometry_msgs-msg:pose (nav_msgs-msg:pose motion-data))))

                  ;; Add observation if human stood still and has turned
                  ;;(when (has-turned (return-orientation-yaw data) last-orientation (turning-angle params))
                    (when (has-turned walking-direction last-walking-direction (turning-angle params)))
                    (let  ((duration (+ last-duration (- (roslisp:ros-time) last-stop-time))))

                    ;; When observation spatially ignored, add duration of last obervation to current one
                      (if (> loc-obs-dist 0.4)
                          (if (> (+ last-duration duration) 0.2)
                              (progn
                                (add-observation-to-hmm
                                 motion-data
                                 objects-cache
                                 full-spatial-model
                                 duration
                                 hmm)
                                (setf last-duration 0)
                                (setf plan-probs (calculate-plan-probabilities (belief hmm)))

                                ;; Incorporate object detections
                                ;; (setf plan-object-hits (calculate-object-confidence-without-locations plan-library objects-cache motion-data full-spatial-model))
                                ;; (setf plan-object-hits (weight-belief-distribution-with-variance plan-object-hits))
                                ;; (setf merged-plan-probs (weight-beliefs plan-probs plan-object-hits))

                                (visualize-plan-probs plan-probs)
                                (write-plan-probs-to-csv plan-probs "~/Desktop/plan-probs.csv")
                                ;; (write-loc-probs-to-csv
                                ;;  (normalize-belief (forward-step-belief hmm)) "/home/kargm/Desktop/loc-probs.csv")

                                ;; TEST: FEEDBACK LOOP for object detections !!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                ;; (format t "Belief before:~%")
                                ;; (maphash #'print-hash-entry (belief hmm))
                                ;; (setf hmm (make-instance 'hmm :belief (normalize-belief (bias-belief-with-plan-probs (belief hmm) merged-plan-probs))
                                ;;                          :start-belief (start-belief hmm)
                                ;;                          :state-transitions (state-transitions hmm)
                                ;;                          :observation-probabilities (observation-probabilities hmm)))
                                ;; (format t "Belief after:~%")
                                ;; (maphash #'print-hash-entry (belief hmm))
                                
                                ;; (format t "Plan Probs:~%")
                                ;; (maphash #'print-hash-entry plan-probs)
                                ;; (format t "Object Hits:~%")
                                ;; (maphash #'print-hash-entry plan-object-hits)
                                ;; (format t "Merged Plan Probs:~%")
                                ;; (maphash #'print-hash-entry merged-plan-probs)
                                
                                ;; (format t "~%Weighted Object Hits:~%")
                                ;; (maphash #'print-hash-entry (weight-belief-distribution-with-variance plan-object-hits))

                                ;;string= location-observation (last-observation hmm)

                                
                                ;; TODOTODOTODO: Get monitoring together in own functions!!!!
                                ;; (write-loc-probs-to-csv
                                ;;  (normalize-belief (forward-step-belief hmm)) "/home/kargm/Desktop/loc-probs.csv")
                                (setf last-orientation (return-orientation-yaw data))
                                (setf last-walking-direction walking-direction)
                                (setf good-plan-observations (get-good-observations
                                                              good-plan-observations
                                                              plan-probs
                                                              0.20
                                                              motion-data
                                                              last-motion-data
                                                              loc-str-table
                                                              plan-library))
                                (write-good-plan-obs-to-csv good-plan-observations "~/Desktop/good-plan-obs.csv")
                                (setf monitoring-belief
                                      (predict-locations
                                       good-plan-observations
                                       plan-library
                                       plan-probs
                                       loc-str-table))
                                
                                 ;; (format t "_____ Good Observations:~%")
                                ;; (maphash #'print-hash-entry good-plan-observations)

                                ;; (format t "_____ Belief HMM:~%")
                                ;; (maphash #'print-hash-entry (normalize-belief (forward-step-belief hmm)))
                                ;; (format t "_____ Monitoring Belief:~%")
                                ;; (maphash #'print-hash-entry monitoring-belief)

                                ;; WARNING!!! merge-beliefs only works in THIS DIRECTION sincen merge-belief needs a hashtable
                                ;; with ":test 'equalp" at second position
                                ;; NOTE: HERE merged-belief is ONLY USED FOR LOCATION-PREDICTION!!!! Could later be used
                                ;; to IMPROVE BELIEF OF HMM!?

                                (unless (eq merged-belief NIL)
                                  (write-loc-probs-to-csv
                                   merged-belief "~/Desktop/loc-probs.csv"))
                                (setf merged-belief
                                      (normalize-belief
                                       (penalyze-beliefs (normalize-belief (forward-step-belief hmm)) monitoring-belief)))
                                (write-loc-probs-to-csv
                                 merged-belief "~/Desktop/loc-probs.csv")
                                ;; (setf merged-belief (normalize-belief (merge-beliefs
                                ;;                                        monitoring-belief
                                ;;                                        (normalize-belief (forward-step-belief hmm)))))
                                ;; (format t "_____ Merged Belief:~%")
                                ;; (maphash #'print-hash-entry merged-belief)
                                
                                ;; Reset object cache only when observations has been added!
                                (setf objects-cache NIL))) ;; When obs added, reset last-duration
                          (progn
                            ;; (format t "Ignored observation [SPATIAL] ~%")
                            (setf last-duration (+ duration last-duration)))))))
                  ;; reset stop-time
                  (setf last-stop-time NIL)))))
      (setf last-data data)
      (setf last-pose
            (geometry_msgs-msg:position (geometry_msgs-msg:pose (nav_msgs-msg:pose data)))))))