(in-package "TSP")

(defparameter *default-data-directory*
  (merge-pathnames
    (make-pathname :directory '(:relative "arbeit" "projekte" "search" "ges" "data"))
    (user-homedir-pathname)))
(defparameter *default-output-directory*
  (merge-pathnames
    (make-pathname :directory '(:relative "arbeit" "docs" "projects" "others" "search" "general-tsps"))
    (user-homedir-pathname)))


