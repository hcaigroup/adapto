(in-package :ad-exe)

;; This file contains motion patterns that are used to detect different
;; situations in motion tracking data

(defun has-turned (orientation last-orientation yaw-angle)
  "Returns true if the absolute difference between ORIENTATION and LAST-ORIENTATION
   is bigger than YAW-ANGLE degree."
  (let ((has-turned NIL))
    (when (> (abs (- orientation last-orientation)) yaw-angle)
      ;; (format t "################ Orientation change: ~s ################~%"
      ;;          (- orientation last-orientation))
      (setf has-turned T))))

(defun get-walking-direction-yaw (pose last-pose)
  "Calculates walking direction of the human given his LAST-POSE and his
   current POSE."
  (atan (-
         (geometry_msgs-msg:y
          (geometry_msgs-msg:position
           (geometry_msgs-msg:pose
            (nav_msgs-msg:pose pose))))
         (geometry_msgs-msg:y
          (geometry_msgs-msg:position
           (geometry_msgs-msg:pose
            (nav_msgs-msg:pose last-pose)))))
        (-
         (geometry_msgs-msg:x
          (geometry_msgs-msg:position
           (geometry_msgs-msg:pose
            (nav_msgs-msg:pose pose))))
         (geometry_msgs-msg:x
          (geometry_msgs-msg:position
           (geometry_msgs-msg:pose
            (nav_msgs-msg:pose last-pose)))))))

(defun post-walking-direction (publisher data yaw)
  "Posts the walking direction of the human as a nav_msgs/Odometry"
  (let ((orientation (cl-transforms:euler->quaternion :az yaw)))
  (roslisp:publish publisher
                   (roslisp:modify-message-copy data
                                                (orientation pose pose)
                                                (roslisp:make-msg "geometry_msgs/Quaternion"
                                                                  z
                                                                  (cl-transforms:z orientation)
                                                                  w
                                                                  (cl-transforms:w orientation))))))