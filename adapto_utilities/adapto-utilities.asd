(defsystem adapto-utilities
  :depends-on (cram-utilities)
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "string" :depends-on ("package"))
             (:file "number" :depends-on ("package"))
             (:file "list" :depends-on ("package" "number"))
             (:file "global-structures" :depends-on ("package"))))))
