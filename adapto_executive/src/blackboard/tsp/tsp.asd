
;;; *****************************************************************************
;;; ** PACKAGE DEFINITION                                                      **
;;; *****************************************************************************
(defpackage "TSP"
  (:documentation "TSP Solving with Blackboard brchitecture")
  (:use "COMMON-LISP" "BB")
  (:import-from "SB-EXT" "QUIT"))


;;; *****************************************************************************
;;; ** ASDF definition                                                         **
;;; *****************************************************************************

(in-package "TSP")

(asdf:defsystem tsp
  :name "tsp"
  :author "Alexandra Kirsch <kirsch@in.tum.de>"
  :version "1.0"
  :maintainer "Alexandra Kirsch <kirsch@in.tum.de>"
  :licence "unknown"
  :description "TSP"
  :long-description "Heuristic approach to solving TSPs with a Blackboard architecture"

  :components
    ( (:module "src" :default-component-class asdf:lisp-source-file
        :components
        ( (:file "config")
          (:module "problem-definition"
            :components
            ( (:file "points")
              (:file "problems")
              (:file "problem-knowledge") )
            :serial T)
          (:file "blackboard-data")
          (:file "solve-tsps")
          (:module "experts"
            :components
            ( (:file "process-types")
              (:file "coordinator-expert")
              (:file "nearest-neighbour")
              (:file "abstraction"))
            :serial T) )
        :serial T) ))