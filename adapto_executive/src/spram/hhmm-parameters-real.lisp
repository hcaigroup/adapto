(in-package :ad-exe)

;;; This file contains the parameter matrices learned from the Garching-Thorben dataset
;;; DW = drink_water, STC = set_table_cornflakes, STQ = set_table_quark,
;;; CT = clean_table, PW = prepare_for_work
;;; NOTE: Only state transitions from the model appear!
;;; They are calculated such that if no hash-entry can be found,
;;; the probability for any other transition is 0.1
;;; NOTE: Table hand-coded for now, should be learned using Baum-Welch or similar

(defun create-state-transition-probabilities ()
  (let ((state-transitions (make-hash-table :test 'equalp)))
    (format t "Setting state transition probabilities... ")
    ;; drink water plan
    (setf (gethash (string 'DW-bottle-place-DW-table) state-transitions) 0.99)
    (setf (gethash (string 'DW-table-DW-cupboard1) state-transitions) 0.33)
    (setf (gethash (string 'DW-cupboard1-DW-table) state-transitions) 0.99)

    ;; transition: drink - set table
    (setf (gethash (string 'DW-table-STC-oven) state-transitions) 0.33)
    (setf (gethash (string 'DW-table-STQ-refrigerator) state-transitions) 0.33)
    
    ;; set table cornflakes
    (setf (gethash (string 'STC-oven-STC-table) state-transitions) 0.99)
    (setf (gethash (string 'STC-table-STC-refrigerator) state-transitions) 0.2475)
    (setf (gethash (string 'STC-refrigerator-STC-table) state-transitions) 0.99)
    (setf (gethash (string 'STC-table-STC-cupboard0) state-transitions) 0.2475)
    (setf (gethash (string 'STC-cupboard0-STC-table) state-transitions) 0.99)
    (setf (gethash (string 'STC-table-STC-drawer) state-transitions) 0.2475)
    (setf (gethash (string 'STC-drawer-STC-table) state-transitions) 0.99)

    ;; set table quark
    (setf (gethash (string 'STQ-table-STQ-refrigerator) state-transitions) 0.2475)
    (setf (gethash (string 'STQ-refrigerator-STQ-table) state-transitions) 0.99)
    (setf (gethash (string 'STQ-table-STQ-cupboard0) state-transitions) 0.2475)
    (setf (gethash (string 'STQ-cupboard0-STQ-table) state-transitions) 0.99)
    (setf (gethash (string 'STQ-table-STQ-drawer) state-transitions) 0.2475)
    (setf (gethash (string 'STQ-drawer-STQ-table) state-transitions) 0.99)

    ;; transition set-table - clean-table
    (setf (gethash (string 'STQ-table-CTQ-refrigerator) state-transitions) 0.2475)
    (setf (gethash (string 'STC-table-CTC-oven) state-transitions) 0.2475)
    
    ;; clean table cornflakes
    (setf (gethash (string 'CTC-table-CTC-sink) state-transitions) 0.7425)
    (setf (gethash (string 'CTC-table-CTC-refrigerator) state-transitions) 0.2475)
    (setf (gethash (string 'CTC-oven-CTC-table) state-transitions) 0.92)
    (setf (gethash (string 'CTC-refrigerator-CTC-table) state-transitions) 0.99)
    (setf (gethash (string 'CTC-sink-CTC-table) state-transitions) 0.495)

    ;; clean table quark
    (setf (gethash (string 'CTQ-table-CTQ-sink) state-transitions) 0.7425)
    (setf (gethash (string 'CTQ-table-CTQ-refrigerator) state-transitions) 0.2475)
    (setf (gethash (string 'CTQ-refrigerator-CTQ-table) state-transitions) 0.99)
    (setf (gethash (string 'CTQ-sink-CTQ-table) state-transitions) 0.495) 

    ;; transition clean - prepare work
    (setf (gethash (string 'CTQ-sink-PW-table) state-transitions) 0.495)
    (setf (gethash (string 'CTC-sink-PW-table) state-transitions) 0.495)
    (setf (gethash (string 'CTC-oven-PW-table) state-transitions) 0.077)
    
    ;; prepare work
    (setf (gethash (string 'PW-table-PW-table2) state-transitions) 0.99)
    (setf (gethash (string 'PW-table2-PW-door) state-transitions) 0.99)
    (format t "Created state-transitions-table: ~%")
    (return-from create-state-transition-probabilities state-transitions)))

;; Set prob to 0.01 if not found
(defun get-state-transition-probability (state1 state2 state-transitions)
  (let ((transition (concatenate 'string (string state1) "-" (string state2)) ))
    (or (gethash transition state-transitions)
      0.01)))

;; Umbrella example
;; (defun create-state-observation-probabilities ()
;;   (let ((observation-probabilities (make-hash-table :test 'equalp)))
;;     (format t "Setting observation probabilities ...")
;;     (setf (gethash (string 'rain-umbrella) observation-probabilities) 0.9)
;;     (setf (gethash (string 'rain-noumbrella) observation-probabilities) 0.1)
;;     (setf (gethash (string 'sun-umbrella) observation-probabilities) 0.2)
;;     (setf (gethash (string 'sun-noumbrella) observation-probabilities) 0.8)
;;     observation-probabilities))
;; (defun create-uniform-state-probabilities ()
;;   (let ((state-probabilities (make-hash-table :test 'equalp)))
;;     (format t "Setting uniform state probabilities ...")
;;     (setf (gethash (string 'DW-rain) state-probabilities) 0.5)
;;     (setf (gethash (string 'DW-sun) state-probabilities) 0.5)
;;     state-probabilities))
;; (defun create-state-transition-probabilities ()
;;   (let ((state-transitions (make-hash-table :test 'equalp)))
;;     (format t "Setting state transition probabilities... ")
;;     (setf (gethash (string 'DW-rain-DW-rain) state-transitions) 0.7)
;;     (setf (gethash (string 'DW-rain-DW-sun) state-transitions) 0.3)
;;     (setf (gethash (string 'DW-sun-DW-sun) state-transitions) 0.7)
;;     (setf (gethash (string 'DW-sun-DW-rain) state-transitions) 0.3)
;;     state-transitions))

;;Observation probabilities are learned from simulation using ML 
;;Others should return p = 0.005
(defun create-state-observation-probabilities ()
  (let ((observation-probabilities (make-hash-table :test 'equalp)))
    (format t "Setting observation probabilities ...")
    ;; table
    (setf (gethash (string 'table-table) observation-probabilities) 0.99)

    (setf (gethash (string 'door-door) observation-probabilities) 0.99)

    (setf (gethash (string 'table2-table2) observation-probabilities) 0.99)
    
    ;; cupboard1
    (setf (gethash (string 'cupboard1-cupboard0) observation-probabilities) 0.69)
    (setf (gethash (string 'cupboard1-bottle-place) observation-probabilities) 0.23)
    (setf (gethash (string 'cupboard1-cupboard1) observation-probabilities) 0.07)

    ;; cupboard0
    (setf (gethash (string 'cupboard0-cupboard0) observation-probabilities) 0.85)
    (setf (gethash (string 'cupboard0-cupboard1) observation-probabilities) 0.08)
    (setf (gethash (string 'cupboard0-sink) observation-probabilities) 0.08)

    ;; sink
    (setf (gethash (string 'sink-sink) observation-probabilities) 0.84)
    (setf (gethash (string 'sink-bottle-place) observation-probabilities) 0.03)
    (setf (gethash (string 'sink-cupboard0) observation-probabilities) 0.13)

    ;; bottle place
    (setf (gethash (string 'bottle-place-bottle-place) observation-probabilities) 0.38)
    (setf (gethash (string 'bottle-place-drawer) observation-probabilities) 0.31)
    (setf (gethash (string 'bottle-place-cupboard1) observation-probabilities) 0.23)
    (setf (gethash (string 'bottle-place-cupboard1) observation-probabilities) 0.08)
    
    ;; drawer
    (setf (gethash (string 'drawer-drawer) observation-probabilities) 0.38)
    (setf (gethash (string 'drawer-cupboard1) observation-probabilities) 0.46)
    (setf (gethash (string 'drawer-bottle-place) observation-probabilities) 0.15)
    
    ;; oven
    (setf (gethash (string 'oven-oven) observation-probabilities) 0.82)
    (setf (gethash (string 'oven-bottle-place) observation-probabilities) 0.06)
    (setf (gethash (string 'oven-drawer) observation-probabilities) 0.06)
    (setf (gethash (string 'oven-refrigerator) observation-probabilities) 0.06)
    
    ;; fridge
    (setf (gethash (string 'refrigerator-refrigerator) observation-probabilities) 0.71)
    (setf (gethash (string 'refrigerator-oven) observation-probabilities) 0.29)
    
    (format t " DONE.~%")
    (return-from  create-state-observation-probabilities observation-probabilities)))

;; If key not found, set to 0.001
(defun get-observation-probability (state observation observation-probs)
  (let ((state-observation "") (plan-independent-state ""))
    (setf plan-independent-state
          (subseq state (+ 1 (position #\- state))))
    (setf state-observation
          (concatenate 'string plan-independent-state "-"
                       (string observation)))
    (or (gethash state-observation observation-probs)
        0.001)))

(defun read-observations-from-file (filename)
  "Reads OBSERVATIONS from an obs.txt file to pass them on to Viterbi"
  (let ((observations NIL) (observation NIL) (last-observation NIL))
    (format t "Reading file ~s~%" filename)
    (with-open-file (stream filename)
    (do ((line (read-line stream nil)
               (read-line stream nil)))
        ((null line))
      (setf observation (subseq line
                                (+ 1 (position #\" line))
                                (position #\" line :from-end T)) )
      (unless (string= (string observation) last-observation)
        (setf observations (append observations (list (string observation)))))
      (setf last-observation (string observation))))
    (format t "Found ~s observations.~%" (length observations))
    observations))

(defun test-hhmm (file-observations)
  (let ((observations '( SINK TABLE TABLE2 DOOR))
        (plan-probs NIL)
        (hmm (make-instance 'hmm)))

    (unless (eq file-observations NIL)
      (setf observations file-observations)
      (format t "++++++++++++++ FOUND OBSERVATIONS! OVERWRITING!!! ++++++++++++~%"))
    ;; init hmm uniformly 
    ;; (setf (belief hmm) (create-uniform-state-probabilities))
    ;; (setf (start-belief hmm) (create-uniform-state-probabilities))
    (setf (belief hmm)
          (create-uniform-state-probabilities-from-stpr-library (generate-stpr-library)))
    (setf (start-belief hmm)
          (create-uniform-state-probabilities-from-stpr-library (generate-stpr-library)))
    (setf (state-transitions hmm) (create-state-transition-probabilities))
    (setf (observation-probabilities hmm) (create-state-observation-probabilities))
    (setf (observations hmm) observations)

    (format t "~%~% --------------------------- HMM INIT DONE --------------------------------~%")

    (forward-backward hmm)
    (dolist (observation observations)
      (format t "~%~%** Adding observation ~s~%" observation)
      
      ;; (format t "Normalized new belief is:~%")
      ;; (maphash #'print-hash-entry new-belief)
      ;; Calculate plan probabilities (1st layer of HHMM)
      (setf plan-probs (calculate-plan-probabilities
                        (normalize-belief (bayes-filter hmm observation)) ))
      (write-plan-probs-to-csv plan-probs "/home/kargm/Desktop/plan-probs.csv")
      (maphash #'print-hash-entry plan-probs)
      (setf (belief hmm) (normalize-belief (bayes-filter hmm observation)))
      ;; (format t "Forward-step:~%")
      ;; (maphash #'print-hash-entry (forward-step-belief hmm))
      )
    (format t "Viterbi:~%")
    (viterbi hmm)))