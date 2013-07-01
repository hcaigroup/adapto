(in-package "CL-USER")

(declaim (optimize (debug 0) (safety 0) (speed 3) (space 0)))

(require :asdf)

; Alt-x ->  slime-sbcl
; (oder mit Ctrl-L slime-ros laden, stört auch nicht)

(push
  (merge-pathnames
    (make-pathname :directory '(:relative "arbeit" "projekte" "cram-adapto" "lisp-external" "asdf-systems"))
    (user-homedir-pathname))
  asdf:*central-registry*)

(push
  (merge-pathnames
    (make-pathname :directory '(:relative "arbeit" "projekte" "search" "blackboard" "asdf-systems"))
    (user-homedir-pathname))
  asdf:*central-registry*)

(require :cl-ppcre)
(require :utilities)
(require :bbarchitecture)
(require :ai-techniques)
(require :tsp)

(in-package "TSP")
