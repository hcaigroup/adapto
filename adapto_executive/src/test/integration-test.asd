(defsystem integration-test
  :depends-on (adapto-executive
               designators
               kdl_arm_kinematics-srv
               ;; unit testing framework also included in sbcl
               #+sbcl sb-rt
               #-sbcl rtest
               )
  :components
  ((:file "package")
   (:file "umbrella-example" :depends-on ("package"))
   (:file "tutorial-tests-suite" :depends-on ("package"
                                              "umbrella-example"))))