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
  (init-expectations)
  
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
      ;; (start-expectation-validation)
      )))




;; Navigate to 3 points in apartment and monitor time of navigation-action
(def-top-level-plan apartment-patrol-task()
  (startup-ros)
  (start-statevar-update)
  (init-expectations)
  
  (create-apartment-object-expectations)
  (create-apartment-human-expectations)
  (create-apartment-world-expectations)

  (with-designators 
      (( loc0-desig 
         (location `((pose 
                      ,(tf:make-pose-stamped "/map" 0.0 
                                             (tf:make-3d-vector -0.29 1.1 0.0) 
                                             (tf:euler->quaternion :az -0.36))))))
       ( loc1-desig 
         (location `((pose 
                      ,(tf:make-pose-stamped "/map" 0.0 
                                             (tf:make-3d-vector 1.386 -0.647 0.0) 
                                             (tf:euler->quaternion :az -1.565))))))
       ( loc2-desig 
         (location `((pose 
                      ,(tf:make-pose-stamped "/map" 0.0 
                                             (tf:make-3d-vector -0.85 -2.05 0.0) 
                                             (tf:euler->quaternion :az 0.121))))))
       ( loc3-desig 
         (location `((pose 
                      ,(tf:make-pose-stamped "/map" 0.0 
                                             (tf:make-3d-vector 4.338 -1.259 0.0) 
                                             (tf:euler->quaternion :az 1.874))))))
       ( loc4-desig 
         (location `((pose 
                      ,(tf:make-pose-stamped "/map" 0.0 
                                             (tf:make-3d-vector -5.117 -2.259 0.0) 
                                             (tf:euler->quaternion :az 1.748)))))))
    
    (par
      (maybe-run-process-modules)
      (start-navigation-watchdog)
      (start-continual-expectation-validation 2)
      (seq
        (cram-process-modules:pm-execute :navigation loc0-desig)
        (sleep 2)
        (cram-process-modules:pm-execute :navigation loc1-desig)
        (sleep 2)
        (cram-process-modules:pm-execute :navigation loc2-desig)
        (sleep 2)
        (cram-process-modules:pm-execute :navigation loc3-desig)
        (sleep 2)
        (cram-process-modules:pm-execute :navigation loc4-desig)
        (sleep 2)
        (cram-process-modules:pm-execute :navigation loc1-desig)
        (sleep 2)
        (cram-process-modules:pm-execute :navigation loc2-desig)
        (sleep 2)
        (cram-process-modules:pm-execute :navigation loc3-desig)
        (sleep 2)
        (cram-process-modules:pm-execute :navigation loc4-desig)))))

(defun create-apartment-object-expectations ()
  ;; NEEDED TO INIT VALUES OF KITCHEN OBJECTS! TODO: BETTER SOLUTION!!!
  (addgv :kitchen-object 'TV (create-object 'TV "thing" 0 0 0 0 0 0 1))
  (setf (last-detection (value (getgv :kitchen-object 'TV)))
        (pose (value (getgv :kitchen-object 'TV))))
  (addgv :expectations 'object-expectations
         (make-instance 'expectations-category
           :expectations-list (list
                               (make-instance 'object-expectation
                                 :object (make-instance 'thing
                                           :pose (fl-funcall #'pose
                                                             (getgv :kitchen-object 'TV))
                                           :last-detection (fl-funcall #'last-detection
                                                                       (getgv :kitchen-object 'TV)))
                                 :flexible NIL)
                               (make-instance 'position-expectation
                                 :area (make-instance 'moving-circle
                                         :radius 2
                                         :x 2.738
                                         :y 1.230)
                                 :pose (fl-funcall #'pose (getgv :kitchen-object 'TV)))))))

(defun create-apartment-human-expectations ()
  (addgv :expectations 'human-expectations
         (make-instance 'expectations-category
           :expectations-list (list
                               (make-instance 'position-expectation
                                 :area (make-instance 'moving-circle
                                         :radius 2
                                         :x -8.2
                                         :y -2)
                                 :pose (fl-funcall #'pose (getgv :human 'louis)))))))

(defun create-apartment-world-expectations ()
  (addgv :doors 'DOOR_ENTRANCE (create-door "DOOR_ENTRANCE" "left" NIL ""))
  (addgv :expectations 'world-expectations
         (make-instance 'expectations-category
           :expectations-list (list
                               (make-instance 'door-expectation
                                 :door-name "DOOR_ENTRANCE"
                                 :expected-open NIL
                                 :is-open (fl-funcall #'door-open
                                                      (getgv :doors 'DOOR_ENTRANCE)))))))

;; NOTE: So far, orientation is not considered
(defun init-kitchen-object-at-location (object-name object-type x y z)
  "Wrapper to create inital expectations for objects at specific locations "
  (format t "--- Initializing object ~s~%" object-name)
  (addgv :kitchen-object object-name (create-object object-name object-type x y z 0 0 0 1))
  (setf (last-detection (value (getgv :kitchen-object object-name)))
                         (pose (value (getgv :kitchen-object object-name)))))

(defun create-object-on-floor-expectation (object-name object-type expected-on-floor)
  "Wrapper for the creation of object-on-floor expectations"
  (make-instance 'object-on-floor-expectation
    :object (make-instance object-type
              :pose (fl-funcall #'pose
                                (getgv :kitchen-object object-name)))
    :expected-on-floor expected-on-floor))

(defun create-static-object-expectation (object-name object-type is-flexible)
  "Wrapper for the creation of static object expectations"
  (make-instance 'object-expectation
    :object (make-instance object-type
              :pose (fl-funcall #'pose
                                (getgv :kitchen-object object-name))
              :last-detection (fl-funcall #'last-detection
                                          (getgv :kitchen-object object-name)))
    :flexible is-flexible))

(defun create-kitchen-objects-expectations ()

  (init-kitchen-object-at-location 'Fork "thing" 0 0 2)
  (init-kitchen-object-at-location 'Knife "thing" 0 0 2)
  (init-kitchen-object-at-location 'Nutella "thing" 0 0 2)
  (init-kitchen-object-at-location 'Jam "thing" 0 0 2)
  (init-kitchen-object-at-location 'Cornflakes "thing" 0 0 2)
  (init-kitchen-object-at-location 'Bowl "thing" 0 0 2)
  (init-kitchen-object-at-location 'Plate "thing" 0 0 2)
  
  (init-kitchen-object-at-location 'Kitchen_Table "thing" 0 0 2)
  
  (addgv :expectations 'object-expectations
         (make-instance 'expectations-category
           :expectations-list (list
                               (create-object-on-floor-expectation 'Fork 'thing NIL)
                               (create-object-on-floor-expectation 'Knife 'thing NIL)
                               (create-object-on-floor-expectation 'Jam 'thing NIL)
                               (create-object-on-floor-expectation 'Nutella 'thing NIL)
                               (create-object-on-floor-expectation 'Cornflakes 'thing NIL)
                               (create-object-on-floor-expectation 'Bowl 'thing NIL)
                               (create-object-on-floor-expectation 'Plate 'thing NIL)
 
                               (create-static-object-expectation 'Fork 'thing NIL)
                               (create-static-object-expectation 'Knife 'thing NIL)
                               (create-static-object-expectation 'Jam 'thing NIL)
                               (create-static-object-expectation 'Nutella 'thing NIL)
                               (create-static-object-expectation 'Cornflakes 'thing NIL)
                               (create-static-object-expectation 'Bowl 'thing NIL)
                               (create-static-object-expectation 'Plate 'thing NIL)
                               
                               (create-static-object-expectation 'Kitchen_Table 'thing NIL)))))

;; Monitor the kitchen table
(def-top-level-plan apartment-kitchen-task()
  (startup-ros)
  (start-statevar-update)
  (init-expectations)
  (create-kitchen-objects-expectations)
  
  (par
     (start-continual-expectation-validation 1)))

;; Plan for Garching Test scenario: Navigate while using SPRAM module to estimate
;; probabilities about human task execution and use those to generate expectations
(def-top-level-plan garching-task()
  ;; (start-statevar-update)
  (startup-ros)
  (init-expectations)
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
        (cram-process-modules:pm-execute :navigation loc1-desig)))))

(def-top-level-plan garching-minimal()
  ;; (start-statevar-update)
  (startup-ros)
  ;; (create-global-structure :activity-expectations)
  (init-expectations)
  ;; (generate-location-expectations)
  (start-observation-watchdog))