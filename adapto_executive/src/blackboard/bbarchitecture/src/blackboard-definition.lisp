(in-package "BB")

;;; Representation Blackboard: hash-Tabelle mit Eintrag pro Chunk Datentyp (d.h. der Typ der im Chunk unter data steht)
;;; pro Chunk-Typ gibt es eine Liste von Prozessen, die Nachrichten dieses Typs erhalten wollen
;;; zusätzlich alle aktuell auf dem Blackboard liegenden Chunks in einer Liste (z.B. wichtig um Blackboard "abzuwischen")

(defclass blackboard ()
  ( (processes-by-chunktype :initform (make-hash-table) :reader processes-by-chunktype)
    (registered-processes :initform () :reader registered-processes)
    (blackboard-chunks :initform () :reader blackboard-chunks)
    (backtracking-stack :initform () :reader backtracking-stack)
    (last-blackboard-status :initform nil :reader last-blackboard-status) ))

;;; Verwaltung Blackboard: es kann ein globales BB geben, dieses wird hier im let gehalten
;;; ansonsten gibt es die Möglichkeit sich das BB ausgeben zu lassen und selbst zu verwalten, z.B. wenn man mehrere verwendet
;;; das globale BB sollte man nur für "Endandwendungen" verwenden, nicht für "Tools" wie Suchfunktionalität, weil die dann mit einem eventuell existierenden globalen BB kollidieren würde


(defmacro with-blackboard (bb &body body)
  `(with-slots (processes-by-chunktype registered-processes blackboard-chunks backtracking-stack last-blackboard-status) ,bb
    ,@body))
;    ,bb))
