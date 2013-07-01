(in-package "BB")

(let ( (global-bb nil) )
  
  ;; Blackboard erzeugen
  (defun make-blackboard (&key (use-as-global-bb nil))
    (let ( (new-bb (make-instance 'blackboard)) )
      (when use-as-global-bb
        (if global-bb
          (error "A global blackboard is already set. Call delete-global-blackboard first to delete the existing blackboard. New blackboard is not set to default.")
          (setf global-bb new-bb)))
      new-bb))

  (defun delete-global-blackboard ()
    (setf global-bb nil))

  ;; Prozesse registrieren
  ;; Added process to registered processes, then traverses processes-by-chunktype and either adds
  ;; the process to the list of the chunktype or creates new chunktype - process entry
  (defun register-process (proc chunk-type &optional (bb global-bb))
    (with-blackboard bb
      (pushnew proc registered-processes)
      (let ( (chunk-type-entry (gethash chunk-type processes-by-chunktype)) )
        (setf (gethash chunk-type processes-by-chunktype)
              (if chunk-type-entry (cons proc chunk-type-entry)
                                   (list proc))))))


  ;; unregister-process-chunktype: eine Chunk-Art für einen Prozess löschen (wenn das die letzte
  ;; Art von Chunk für diesen Prozess war, wir der ganze Prozess entfernt; als chunk-type kann :all
  ;; angegeben werden, das ergibt dann die Funktionalität von unregister-process
  (defun unregister-process-chunktype (proc chunk-type &optional (bb global-bb))
    (with-blackboard bb
      (case chunk-type
        (:all (maphash #'(lambda (ct proclist)
                           (declare (ignore proclist))
                           (unregister-process-chunktype proc ct))
                       processes-by-chunktype)
              (setf registered-processes (remove proc registered-processes)))
        (otherwise (let*( (chunk-type-entry (gethash chunk-type processes-by-chunktype))
                          (new-chunk-type-entry (remove proc chunk-type-entry)) )
                     (if (null new-chunk-type-entry)
                       (remhash chunk-type processes-by-chunktype)
                       (setf (gethash chunk-type processes-by-chunktype) new-chunk-type-entry)))
                   (let ( (proc-occurred-p nil) )
                     (maphash #'(lambda (ct proclist) (when (member proc proclist) (setf proc-occurred-p T)))
                              processes-by-chunktype))))))

  (defun unregister-process (proc &optional (bb global-bb))
    (unregister-process-chunktype proc :all bb))


  ; show-blackboard: for debugging
  (defun show-blackboard (&optional (bb global-bb))
    (with-blackboard bb
      (let ( (output nil) )
        (maphash #'(lambda (chunk procs) (push (list chunk procs) output)) processes-by-chunktype)
        (values output blackboard-chunks))))

  ;; Daten verwalten
  ;; Traverse hashtable processes-by-chunktype, check if current chunk == chunk-type and set
  ;; processes to inform to list proclist.
  (defun inform-processes-of-new-chunk (chunk &optional (bb global-bb))
    (with-blackboard bb
      (let ( (procs ()) )
        (maphash #'(lambda (ct proclist) (when (typep (data chunk) ct) (setf procs (union proclist procs))))
                 processes-by-chunktype)
        (mapcar #'(lambda (p) (inform-process p chunk)) procs))))

  (defun add-chunk-to-bb (chunk &optional (bb global-bb))
    (with-blackboard bb
      (push chunk blackboard-chunks)
      (inform-processes-of-new-chunk chunk bb)))

  (defun chunk-modified-on-bb (chunk &optional (bb global-bb))
    (with-blackboard bb
      (inform-processes-of-new-chunk chunk bb)))

  (defun remove-chunk-from-bb (chunk &optional (bb global-bb))
    (with-blackboard bb
      (forget-chunk chunk)
      (setf blackboard-chunks (remove chunk blackboard-chunks))))

  (defun wipe-blackboard (&optional (bb global-bb))
    (with-blackboard bb
      (dolist (ch blackboard-chunks) (forget-chunk ch)) ; forget im Moment nicht nötig, aber vielleicht hilfreich, wenn man mal nicht alles löscht, sondern nur bestimmte chunks
      (dolist (proc registered-processes) (clear-chunk-queue proc))
      (setf blackboard-chunks ())))


;; Backtracking Mechanismus: Blackboard komplett speichern bzw. laden
;; (das könnte man schöner machen, dass nur bestimmte Informationen gespeichert werden, aber sollte erstmal tun)
;; hier nicht behandelt: sicherstellen, dass man nicht wieder dieselbe Lösung produziert
;; (sollte besser problemspezifisch behandelt werden, z.B. durch andere Gewichtung von Heuristiken)

  (defun set-backtracking-point (&optional (bb global-bb))
    (with-blackboard bb
      (push blackboard-chunks backtracking-stack)))

  (defun backtrack (&optional (bb global-bb))
    (wipe-blackboard bb)
    (with-blackboard bb
      (setf blackboard-chunks (pop backtracking-stack))
      (dolist (chunk blackboard-chunks)
        (reanimate-chunk chunk)
        (inform-processes-of-new-chunk chunk bb))))

  ; try-backtrack und abort-backtrack:
  ; speichert aktuellen Zustand bevor Backtracking gemacht wird, d.h. wenn man schon eine gute Lösung hatte
  ; und trotz Backtracking nichts besseres findet, kann man wieder zur alten Lösung zurückkehren
  (defun try-backtrack (&optional (bb global-bb))
    (with-blackboard bb
      (setf last-blackboard-status blackboard-chunks)
      (backtrack bb)))

  (defun abort-backtrack (&optional (bb global-bb))
    (with-blackboard bb
      (setf blackboard-chunks last-blackboard-status)
      (dolist (chunk blackboard-chunks)
        (reanimate-chunk chunk)
        (inform-processes-of-new-chunk chunk bb))))

)
