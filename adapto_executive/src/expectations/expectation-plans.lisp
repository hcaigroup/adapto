(in-package :ad-exe)

;; Move Jido and validate expectations
(def-top-level-plan plan-location-expectation()
  (cram-execution-trace:enable-fluent-tracing)
  ;; (cet:enable-auto-tracing)
  (start-statevar-update)
  ;; Note: start-statevar-update is asynchronous! generate-expectations should somehow check if human and/or
  ;; robot is already percieved and when last detection has been!
  (generate-location-expectations)

   (with-designators 
                  (( loc-desig 
                     (location `((pose 
                                 ,(tf:make-pose-stamped "/map" 0.0 
                                   (tf:make-3d-vector 5.6 0.1 0.0) 
                                   (tf:euler->quaternion :az (/ pi 2.0))))))))
             (par
               (maybe-run-process-modules)
               (start-expectation-validation)
               (achieve `(loc robot ,loc-desig))
               ;; (cram-process-modules:pm-execute :navigation loc-desig)
               )))

(def-top-level-plan movable-test-1()
  (start-statevar-update)
  ;; Note: start-statevar-update is asynchronous! generate-expectations should somehow check if human and/or
  ;; robot is already percieved and when last detection has been!
  (generate-object-expectations)

   (with-designators 
                  (( loc-desig 
                     (location `((pose 
                                 ,(tf:make-pose-stamped "/map" 0.0 
                                   (tf:make-3d-vector -5.739 -1.206 0.01) 
                                   (tf:euler->quaternion :az (/ pi 2.0))))))))
             (par
               (maybe-run-process-modules)
               (start-expectation-validation)
               (cram-process-modules:pm-execute :navigation loc-desig))))

;; Generate watchdog that waits until a navigation process module executes an action
;; and  then generate an navigation expectation


;; Navigate to 3 points and monitor time of navigation-action
(def-top-level-plan navigation-task()
  (start-statevar-update)
  (create-global-structure :expectations)
  ;; (generate-location-expectations)
  
  (with-designators 
      (( loc1-desig 
         (location `((pose 
                      ,(tf:make-pose-stamped "/map" 0.0 
                                             (tf:make-3d-vector -3.7 3 0.0) 
                                             (tf:euler->quaternion :az (/ pi 1)))))))
       ( loc2-desig 
         (location `((pose 
                      ,(tf:make-pose-stamped "/map" 0.0 
                                             (tf:make-3d-vector 0.5 -2.8 0.0) 
                                             (tf:euler->quaternion :az (/ pi 2.0)))))))

       ( loc3-desig 
         (location `((pose 
                      ,(tf:make-pose-stamped "/map" 0.0 
                                             (tf:make-3d-vector 0.1 2.7 0.0) 
                                             (tf:euler->quaternion :az (/ pi 2.0))))))))
    
    (par
      (maybe-run-process-modules)
      (start-navigation-watchdog)
      (seq 
        (cram-process-modules:pm-execute :navigation loc1-desig)
        (sleep 2)
        (cram-process-modules:pm-execute :navigation loc2-desig)
        (sleep 2)
        (cram-process-modules:pm-execute :navigation loc3-desig))
      (start-expectation-validation))))

;; Navigate to 3 points in apartment and monitor time of navigation-action
(def-top-level-plan apartment-task()
  (start-statevar-update)
  (create-global-structure :expectations)
  ;; (generate-location-expectations)
  
  (with-designators 
      (( loc1-desig 
         (location `((pose 
                      ,(tf:make-pose-stamped "/map" 0.0 
                                             (tf:make-3d-vector 1.386 -0.647 0.0) 
                                             (tf:euler->quaternion :az -1.565))))))
       ( loc2-desig 
         (location `((pose 
                      ,(tf:make-pose-stamped "/map" 0.0 
                                             (tf:make-3d-vector 0.0824 -4.234 0.0) 
                                             (tf:euler->quaternion :az 0.121))))))
       ( loc3-desig 
         (location `((pose 
                      ,(tf:make-pose-stamped "/map" 0.0 
                                             (tf:make-3d-vector 4.207 -0.277 0.0) 
                                             (tf:euler->quaternion :az 1.874))))))
       ( loc4-desig 
         (location `((pose 
                      ,(tf:make-pose-stamped "/map" 0.0 
                                             (tf:make-3d-vector -5.117 -2.259 0.0) 
                                             (tf:euler->quaternion :az 1.748)))))))
    
    (par
      (maybe-run-process-modules)
      (start-navigation-watchdog)
      (seq 
        (cram-process-modules:pm-execute :navigation loc1-desig)
        (sleep 2)
        (cram-process-modules:pm-execute :navigation loc2-desig)
        (sleep 2)
        (cram-process-modules:pm-execute :navigation loc5-desig)
        (sleep 2)
        (cram-process-modules:pm-execute :navigation loc7-desig))
      
      (start-expectation-validation))))

;; Plan for Garching Test scenario: Navigate while using SPRAM module to estimate
;; probabilities about human task execution and use those to generate expectations
(def-top-level-plan garching-task()
  ;; (start-statevar-update)
  (startup-ros)
  (create-global-structure :expectations)
  ;; (generate-location-expectations)
  
  (with-designators 
      (( loc1-desig 
         (location `((pose 
                      ,(tf:make-pose-stamped "/map" 0.0 
                                             (tf:make-3d-vector 3 3 0.0) 
                                             (tf:euler->quaternion :az -1.565)))))))
    (par
      (start-observation-watchdog)      
      (maybe-run-process-modules)
      (seq 
        (cram-process-modules:pm-execute :navigation loc1-desig))
      )))

(def-top-level-plan garching-minimal()
  ;; (start-statevar-update)
  (startup-ros)
  (create-global-structure :expectations)
  ;; (generate-location-expectations)
  (start-observation-watchdog))