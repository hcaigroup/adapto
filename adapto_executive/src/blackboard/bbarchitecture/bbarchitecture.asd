
;;; *****************************************************************************
;;; ** PACKAGE DEFINITION                                                      **
;;; *****************************************************************************
(defpackage "BB"
  (:documentation "Blackboard Architecture")
  (:use "COMMON-LISP")
  (:import-from "SB-EXT" "QUIT")
  (:export
  ; chunk
  "CHUNK" "DATA" "STILL-EXISTS-P"
  ; process
  "PROCESS" "GET-CHUNK-QUEUE" "POP-CHUNK-QUEUE" "CHUNK-QUEUE-EMPTY-P" "CLEAR-CHUNK-QUEUE"
  ; blackboard
  "MAKE-BLACKBOARD" "DELETE-GLOBAL-BLACKBOARD" "SHOW-BLACKBOARD"
  "REGISTER-PROCESS" "UNREGISTER-PROCESS-CHUNKTYPE" "UNREGISTER-PROCESS"
  "ADD-CHUNK-TO-BB" "CHUNK-MODIFIED-ON-BB" "REMOVE-CHUNK-FROM-BB" "WIPE-BLACKBOARD"
  "SET-BACKTRACKING-POINT" "BACKTRACK" "TRY-BACKTRACK" "ABORT-BACKTRACK"))




;;; *****************************************************************************
;;; ** ASDF definition                                                         **
;;; *****************************************************************************

(in-package "BB")

(asdf:defsystem bbarchitecture
  :name "bbarchitecture"
  :author "Alexandra Kirsch <kirsch@in.tum.de>"
  :version "1.0"
  :maintainer "Alexandra Kirsch <kirsch@in.tum.de>"
  :licence "unknown"
  :description "bb architecture"
  :long-description "general blackboard architecture"

  :components
    ( (:module "src" :default-component-class asdf:lisp-source-file
        :components
        ( (:file "chunks")
          (:file "processes")
          (:file "blackboard-definition")
          (:file "blackboard-actions") )
        :serial T) ))