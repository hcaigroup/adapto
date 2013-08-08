(in-package :ad-exe)

(defun generate-duration-exp (location-name max-expected-duration)
  "This function generates a duration prob for the location LOCATION-NAME with
   MAX-EXPECTED-DURATION. Only one duration expectation is allowed, so global
   structure gets cleared before new durations are added."
  (let ((*package* (find-package :ad-exe)))
    (clear-global-structure :human-duration-expectations)
    (addgv :human-duration-expectations (intern location-name)
           (make-instance 'duration-expectation
             :location-name (string location-name)
             :max-expected-duration max-expected-duration
             :time-entered (get-universal-time)))))

(defun generate-loc-exps-from-prob-dist (loc-probs)
  "This funtion creates and updates expectations about the next estimated location of the human
   according to the probability histogram LOC-PROBS"
  (let ((*package* (find-package :ad-exe)))
    (maphash #'(lambda (location probability)
                 (addgv :human-activity-expectations (intern (concatenate 'string location "-future"))
                        (make-instance 'next-location-expectation
                          :next-location-guess (string location)
                          :next-location NIL
                          :weight probability)))
             loc-probs)))

(defun update-next-location (location loc-probs)
  "This function updates all currently active location expectations with the current location
   detection."
  (let ((*package* (find-package :ad-exe)))
    (maphash #'(lambda (loc prob)
                 (declare (ignore prob))
                 (addgv :human-activity-expectations (intern loc)
                        (make-instance 'next-location-expectation
                          :next-location-guess (next-location-guess (getgv :human-activity-expectations (intern (concatenate 'string loc "-future"))))
                          :next-location location
                          :weight (weight (getgv :human-activity-expectations (intern (concatenate 'string loc "-future"))))
                          :ready-for-validation T)))
             loc-probs)))

(defun generate-location-expectations ()
  "Here we define the instances of our expectations and put them into a global structure
   x,y and pose of location expectations are fluents since they are subject to change.
   For example here: An expectation about the human beeing no more than 6 meters away from Jido"
  (init-expectations)
  (addgv :world-physical-expectations 'louis-near-james (make-instance 'position-expectation
                                    :area (make-instance 'moving-circle
                                            :radius 2
                                            :x (fl-funcall #'(lambda (fl-x)
                                                               (tf:x (tf:origin (pose fl-x))))
                                                           (getgv :robot 'james))
                                            :y (fl-funcall #'(lambda (fl-y)
                                                               (tf:y (tf:origin (pose fl-y))))
                                                           (getgv :robot 'james)))
                                    :pose (fl-funcall #'pose (getgv :human 'louis))))

  ;; (addgv :world-physical-expectations 'louis-near-red-cube (make-instance 'position-expectation
  ;;                                             :area (make-instance 'moving-circle
  ;;                                                     :radius 5
  ;;                                                     :x (fl-funcall #'(lambda (fl-x)
  ;;                                                                        (tf:x
  ;;                                                                         (cl-transforms:origin
  ;;                                                                          (pose fl-x))))
  ;;                                                                    (getgv :kitchen-object 'red_cube))
  ;;                                                     :y (fl-funcall #'(lambda (fl-y)
  ;;                                                                        (tf:y (cl-transforms:origin
  ;;                                                                               (pose fl-y))))
  ;;                                                                    (getgv :kitchen-object 'red_cube)))
  ;;                                             :pose (fl-funcall #'pose (getgv :human 'louis))))
  
  ;; (addgv :world-physical-expectations 'louis-near-desk (make-instance 'position-expectation
  ;;                                             :area (make-instance 'moving-circle
  ;;                                                     :radius 5
  ;;                                                     :x (fl-funcall #'(lambda (fl-x)
  ;;                                                                        (tf:x (cl-transforms:origin
  ;;                                                                               (pose fl-x))))
  ;;                                                                    (getgv :kitchen-object 'desk_2))
  ;;                                                     :y (fl-funcall #'(lambda (fl-y)
  ;;                                                                        (tf:y (cl-transforms:origin
  ;;                                                                               (pose fl-y))))
  ;;                                                                    (getgv :kitchen-object 'desk_2)))
  ;;                                             :pose (fl-funcall #'pose (getgv :human 'louis))))
  )

(defun generate-object-expectations ()
  (init-expectations)
  (addgv :object-physical-expectations 'red-cube-static
         (make-instance 'object-expectation
           :object (make-instance 'thing
                     :pose (fl-funcall #'pose
                                       (getgv :kitchen-object 'red_cube)))
           :flexible NIL))
   
  (addgv :object-physical-expectations 'cyan-cube-static
         (make-instance 'object-expectation
           :object (make-instance 'thing
                     :pose (fl-funcall #'pose
                                       (getgv :kitchen-object 'cyan_cube)))
           :flexible NIL))

  (addgv :object-physical-expectations 'green-cube-static
         (make-instance 'object-expectation
           :object (make-instance 'thing
                     :pose (fl-funcall #'pose
                                       (getgv :kitchen-object 'green_cube)))
           :flexible NIL))

  (addgv :object-physical-expectations 'pink-cube-static
         (make-instance 'object-expectation
           :object (make-instance 'thing
                     :pose (fl-funcall #'pose
                                       (getgv :kitchen-object 'pink_cube)))
           :flexible NIL))
  
  (addgv :object-physical-expectations 'yellow-cube-static
         (make-instance 'object-expectation
           :object (make-instance 'thing
                     :pose (fl-funcall #'pose
                                       (getgv :kitchen-object 'yellow_cube)))
           :flexible NIL))
  
  (addgv :object-physical-expectations 'purple-cube-static
         (make-instance 'object-expectation
           :object (make-instance 'thing
                     :pose (fl-funcall #'pose
                                       (getgv :kitchen-object 'purple_cube)))
           :flexible NIL)))

(defun clean-expectations ()
  (clear-global-structure :robot-navigation-expectations)
  (clear-global-structure :world-physical-expectations)
  (clear-global-structure :object-physcial-expectations)
  (clear-global-structure :human-duration-expectations)
  (clear-global-structure :human-activity-expectations)
  )

;; This function defines the structure of our expectations-network
(defun init-expectations ()
  (create-global-structure :robot-navigation-expectations)
  (create-global-structure :world-physical-expectations)
  (create-global-structure :object-physical-expectations)
  (create-global-structure :human-duration-expectations)
  (create-global-structure :human-activity-expectations))