(in-package :ad-exe)

(defun generate-duration-exp (location-name max-expected-duration)
  "This function generates a duration prob for the location LOCATION-NAME with
   MAX-EXPECTED-DURATION. Only one duration expectation is allowed, so global
   structure gets cleared before new durations are added."
   (make-instance 'duration-expectation
     :location-name (string location-name)
     :max-expected-duration max-expected-duration
     :time-entered (get-universal-time)))

(defun generate-loc-exps-from-prob-dist (loc-probs)
  "This funtion creates and updates expectations about the next estimated location of the human
   according to the probability histogram LOC-PROBS"
  (make-instance 'next-location-expectation
    :next-location-probdist loc-probs
    :next-location NIL))

(defun create-or-update-duration-expectation (location-name max-expected-duration)
  "This function updates the duration expectation with new values or creates a new one of no
   duration expectation does exist. Returns expectations category."
  (let ((*package* (find-package :ad-exe)))
    (setgv :expectations 'human-expectations
           (add-or-update-expectation
            (getgv :expectations 'human-expectations)
            'DURATION-EXPECTATION
            (make-instance 'duration-expectation
              :location-name (string location-name)
              :max-expected-duration max-expected-duration
              :time-entered (get-universal-time))))))

(defun create-or-update-loc-exps-from-prob-dist (loc-probs location-observation)
  "This function updates all currently active next-location expectations with the current location
   detection. Returns expectations category"
  (let ((*package* (find-package :ad-exe))
         (human-expectations (getgv :expectations 'human-expectations)))
    (unless (null (gethash 'NEXT-LOCATION-FUTURE (expectations-table human-expectations)))
    (add-or-update-expectation
     human-expectations
     'NEXT-LOCATION-PAST
     (make-instance 'next-location-expectation
       :next-location-probdist (next-location-probdist
                                (gethash 'NEXT-LOCATION-FUTURE
                                         (expectations-table
                                          human-expectations)))
       :next-location location-observation
       :ready-for-validation T))
      )

    (add-or-update-expectation
     human-expectations
     'NEXT-LOCATION-FUTURE
     (make-instance 'next-location-expectation
       :next-location-probdist loc-probs
       :next-location NIL
       :ready-for-validation T))
    human-expectations))

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
  (clear-global-structure :expectations))

;; This function defines the structure of our expectations-network
(defun init-expectations ()
  (create-global-structure :expectations))