(in-package :ad-exe)

;; Subscribe to planner, and get plan of global navigation
;; Length of the array returns strange values...
;; TODO: Iterate through array and calculate lengths between waypoints
(defun calculate-time (data)
  "This function subscribes to the global navigatino planner and calculates uses the path-lenth
   from the DATA of the ROS-msg to calculate the expected navigation-time "
  (let ((distances-waypoints nil) (distance-air nil) (path-length nil))
    ;; (format t "Found path of length ~s~%" (length (nav_msgs-msg:poses data)))

    ;; calculate distance btw. poses of the global plan
    (setf distances-waypoints 
          (loop for i from 1 to (- (length (nav_msgs-msg:poses data)) 1) collect
               (cl-transforms:v-dist
                (cl-transforms::make-3d-vector
                 (geometry_msgs-msg:x (geometry_msgs-msg:position
                                       (geometry_msgs-msg:pose (elt (nav_msgs-msg:poses data) (- i 1)))))
                 (geometry_msgs-msg:y (geometry_msgs-msg:position
                                       (geometry_msgs-msg:pose (elt (nav_msgs-msg:poses data) (- i 1)))))
                 (geometry_msgs-msg:z (geometry_msgs-msg:position
                                       (geometry_msgs-msg:pose (elt (nav_msgs-msg:poses data) (- i 1))))))
                (cl-transforms:make-3d-vector
                 (geometry_msgs-msg:x (geometry_msgs-msg:position
                                       (geometry_msgs-msg:pose (elt (nav_msgs-msg:poses data) i))))
                 (geometry_msgs-msg:y (geometry_msgs-msg:position
                                       (geometry_msgs-msg:pose (elt (nav_msgs-msg:poses data) i))))
                 (geometry_msgs-msg:z (geometry_msgs-msg:position
                                       (geometry_msgs-msg:pose (elt (nav_msgs-msg:poses data) i))))))))

    ;; calculate distance between start- and endpose of the plan (NOT NEEDED AT THE MOMENT!)
    (setf distance-air
          (cl-transforms:v-dist
           (cl-transforms::make-3d-vector
            (geometry_msgs-msg:x
             (geometry_msgs-msg:position
              (geometry_msgs-msg:pose (elt (nav_msgs-msg:poses data) 0))))
            (geometry_msgs-msg:y
             (geometry_msgs-msg:position
              (geometry_msgs-msg:pose (elt (nav_msgs-msg:poses data) 0))))
            (geometry_msgs-msg:z
             (geometry_msgs-msg:position
              (geometry_msgs-msg:pose (elt (nav_msgs-msg:poses data) 0)))))
           (cl-transforms:make-3d-vector
            (geometry_msgs-msg:x
             (geometry_msgs-msg:position
              (geometry_msgs-msg:pose (elt (nav_msgs-msg:poses data)
                                           (- (length (nav_msgs-msg:poses data)) 1)))))
            (geometry_msgs-msg:y
             (geometry_msgs-msg:position
              (geometry_msgs-msg:pose (elt (nav_msgs-msg:poses data)
                                           (- (length (nav_msgs-msg:poses data)) 1)))))
            (geometry_msgs-msg:z
             (geometry_msgs-msg:position
              (geometry_msgs-msg:pose (elt (nav_msgs-msg:poses data)
                                           (- (length (nav_msgs-msg:poses data)) 1))))))))

    (setf path-length (apply '+ distances-waypoints))
    
    ;; Generate an navigation-expectation of it not already exists
    (unless (isgv :expectations 'robot-expectations)
      (let ((expectations-table (make-hash-table)))
        (setf (gethash 'robot-navigation expectations-table)
              (make-instance 'navigation-action-expectation
                ;; TODO: HERE AVERAGE SPEED SHOULD BE SET!!!!!
                ;; (at the moment just set 0.3)
                :duration (/ path-length 0.15)
                :start-time (roslisp:ros-time)
                :path-length path-length
                :avg-speed 0.15))
            (addgv :expectations 'robot-expectations
               (make-instance 'expectations-category
                 :expectations-table expectations-table))))
    ;; (format t "Sum of waypoint-distances: ~s~%" path-length)
    ;; (format t "Linear-distance:              ~s~%" distance-air)
    ;; (format t "Difference: ~s~% ---------------- ~%" (- (apply '+ distances-waypoints) distance-air))
    (sleep 2)))

(let ((last_navp nil) (subscriber nil))
  (defun start-navigation-watchdog ()
    "This function starts a watchdog that generates an expectation to estimate the expected time
     that the robot will need to get to its goal. An expectation is generated every time, a
     navigation action starts."
    ;; Check if navigation is running
    (unless (eq [cpm:pm-status :navigation] :WAITING)
      ;; (format t "-")
      ;; check if navigaton action has just started and subscribe to navigation plan
      (when (eq last_navp 0)
        ;; (format t "STARTED NAVIGATION, creating expectation:")
        (setf subscriber
              (roslisp:subscribe "/move_base/NavfnROS/plan"
                                 "nav_msgs/Path"
                                 #'calculate-time :max-queue-length 1))))
    
    ;; Check if navigation-action has ended and unsubscribe
    (unless (eq [cpm:pm-status :navigation] :RUNNING)
      (when (eq last_navp 1)
        (roslisp:unsubscribe subscriber)
        (remgv :expectations 'robot-expectations)
        ;; (format t "-:NAVIGATION ENDED, removed expectation")
        ))
    ;; Save last status 
    (if (eq [cpm:pm-status :navigation] :RUNNING)
      (setf last_navp 1)
      (setf last_navp 0))
    (sleep 2)
    (start-navigation-watchdog)))
    