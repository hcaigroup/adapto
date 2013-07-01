(in-package "TSP")

; Datentypen, die auf dem Blackboard landen können

(defclass candidate-point (point)
  ( (evaluation-values :initform () :initarg :evaluation-values :accessor evaluation-values) ))

(defclass ranking ()
  ( (point-list :initform nil :initarg :point-list :accessor point-list) ))


;; Signale in Form von Symbolen
; Signal für nächste Runde nachdem coordinator Prozess sich für einen Punkt entschieden hat
(defun next-signal-p (x)
  (eq x :next-round))

(deftype process-signal `()
  '(satisfies next-signal-p))

(defun make-next-signal ()
  :next-round)

; Stop-Signal
(defun stop-signal-p (x)
  (eq x :stop))

(deftype stop-signal `()
  '(satisfies stop-signal-p))

(defun make-stop-signal ()
  :stop)