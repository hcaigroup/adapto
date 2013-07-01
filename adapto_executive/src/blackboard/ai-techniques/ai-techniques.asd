
;;; *****************************************************************************
;;; ** PACKAGE DEFINITION                                                      **
;;; *****************************************************************************
(defpackage "AIT"
  (:documentation "AI Techniques")
  (:use "COMMON-LISP" "BB")
  (:import-from "SB-EXT" "QUIT"))


;;; *****************************************************************************
;;; ** ASDF definition                                                         **
;;; *****************************************************************************

(in-package "AIT")

(asdf:defsystem ai-techniques
  :name "ai-techniques"
  :author "Alexandra Kirsch <kirsch@in.tum.de>"
  :version "1.0"
  :maintainer "Alexandra Kirsch <kirsch@in.tum.de>"
  :licence "unknown"
  :description "AI techniques"
  :long-description "Implementation of well-known (or lesser known) techniques from AI and cognitive systems with the blackboard architecture"

  :components
    ( (:module "src" :default-component-class asdf:lisp-source-file
        :components
        ( (:file "depth-first-search")
          #| (:file "search-general")
          (:file "pyramid-model")
          (:file "particle-filter") |# )
        #| :serial T |#) ))