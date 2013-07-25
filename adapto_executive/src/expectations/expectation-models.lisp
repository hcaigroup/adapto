(in-package :ad-exe)

(defun generate-loc-exps-from-prob-dist (loc-probs)
  "This funtion creates and updates expectations about the next estimated location of the human
   according to the probability histogram LOC-PROBS"
  (let ((*package* (find-package :ad-exe)))
    (maphash #'(lambda (location probability)
                 (addgv :expectations (intern (concatenate 'string location "-future"))
                        (make-instance 'next-location-expectation
                          :next-location-guess (string location)
                          :next-location NIL
                          :weight probability
                          :ready-for-validation NIL)))
             loc-probs)))

(defun update-next-location (location loc-probs)
  "This function updates all currently active location expectations with the current location
   detection."
  (let ((*package* (find-package :ad-exe)))
    (maphash #'(lambda (loc prob)
                 (declare (ignore prob))
                 (addgv :expectations (intern loc)
                        (make-instance 'next-location-expectation
                          :next-location-guess (next-location-guess (getgv :expectations (intern (concatenate 'string loc "-future"))))
                          :next-location location
                          :weight (weight (getgv :expectations (intern (concatenate 'string loc "-future"))))
                          :ready-for-validation T)))
             loc-probs)))

(defun generate-location-expectations ()
  "Here we define the instances of our expectations and put them into a global structure
   x,y and pose of location expectations are fluents since they are subject to change.
   For example here: An expectation about the human beeing no more than 6 meters away from Jido"
  (create-global-structure :expectations)
  (addgv :expectations 'louis-near-james (make-instance 'position-expectation
                                    :area (make-instance 'moving-circle
                                            :radius 2
                                            :x (fl-funcall #'(lambda (fl-x)
                                                               (tf:x (tf:origin (pose fl-x))))
                                                           (getgv :robot 'james))
                                            :y (fl-funcall #'(lambda (fl-y)
                                                               (tf:y (tf:origin (pose fl-y))))
                                                           (getgv :robot 'james)))
                                    :pose (fl-funcall #'pose (getgv :human 'louis))))

  ;; (addgv :expectations 'louis-near-red-cube (make-instance 'position-expectation
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
  
  ;; (addgv :expectations 'louis-near-desk (make-instance 'position-expectation
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
   (create-global-structure :expectations)
   (addgv :expectations 'red-cube-static
          (make-instance 'object-expectation
            :object (make-instance 'thing
                      :pose (fl-funcall #'pose
                                        (getgv :kitchen-object 'red_cube)))
            :flexible NIL))
   
   (addgv :expectations 'cyan-cube-static
          (make-instance 'object-expectation
            :object (make-instance 'thing
                      :pose (fl-funcall #'pose
                                        (getgv :kitchen-object 'cyan_cube)))
            :flexible NIL))

   (addgv :expectations 'green-cube-static
          (make-instance 'object-expectation
            :object (make-instance 'thing
                      :pose (fl-funcall #'pose
                                        (getgv :kitchen-object 'green_cube)))
            :flexible NIL))

   (addgv :expectations 'pink-cube-static
          (make-instance 'object-expectation
            :object (make-instance 'thing
                      :pose (fl-funcall #'pose
                                        (getgv :kitchen-object 'pink_cube)))
            :flexible NIL))
   
  (addgv :expectations 'yellow-cube-static
         (make-instance 'object-expectation
           :object (make-instance 'thing
                     :pose (fl-funcall #'pose
                                       (getgv :kitchen-object 'yellow_cube)))
           :flexible NIL))
  
   (addgv :expectations 'purple-cube-static
          (make-instance 'object-expectation
            :object (make-instance 'thing
                      :pose (fl-funcall #'pose
                                        (getgv :kitchen-object 'purple_cube)))
            :flexible NIL)))

(defun clean-expectations ()
  (clear-global-structure :expectations))