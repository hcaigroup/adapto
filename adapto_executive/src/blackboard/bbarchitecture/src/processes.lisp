(in-package "BB")

(defclass process ()
  ( (chunk-queue :initform () :accessor chunk-queue) ))

(defgeneric inform-process (process chunk))
(defgeneric pop-chunk-queue (process))
(defgeneric clear-chunk-queue (process))
(defgeneric process-run (process))

; inform-process: write chunk information to chunk queue
(defmethod inform-process ((proc process) (ch chunk))
  (setf (slot-value proc 'chunk-queue)
        (append (slot-value proc 'chunk-queue) (list ch))))

; pop-chunk-queue: retrieve message from chunk queue
(defmethod pop-chunk-queue ((proc process))
  (pop (chunk-queue proc)))

(defmethod chunk-queue-empty-p ((proc process))
  (null (chunk-queue proc)))

; clear-chunk-queue: remove obsolete chunks from queue
(defmethod clear-chunk-queue ((proc process))
  (setf (chunk-queue proc) ()))

;;; hier noch speziellere Prozesse, z.B. Aktionen, Generatoren, Evaluationsexperten/Advisors...

