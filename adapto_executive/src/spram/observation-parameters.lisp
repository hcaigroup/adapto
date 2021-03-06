(in-package :ad-exe)

(defclass parameters ()
  ((objects-ros-topic :initarg :objects-ros-topic :accessor objects-ros-topic)
   (movement-distance :initarg :movement-distance :accessor movement-distance)
   (turning-angle :initarg :turning-angle :accessor turning-angle)
   (human-movement-time :initarg :human-movement-time :accessor human-movement-time)))

(defun get-morse-params ()
  ;; times in secs, distance in meters
  (let ((params (make-instance 'parameters
                  :objects-ros-topic "/james/semantic_camera"
                  :movement-distance 0.01
                  :turning-angle 1.06
                  :human-movement-time 0.5)))
    params))

(defun get-real-params ()
  ;; times in secs, distance in meters
  (let ((params (make-instance 'parameters
                  :objects-ros-topic "/objects_relative_to_map"
                  :movement-distance 0.025
                  :turning-angle 1.06
                  :human-movement-time 0.25)))
    params))