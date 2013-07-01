(in-package :ad-exe)

;;; This file contains functions for creating and using a Hierarchical Hidden Markov Model for
;;; Activity Recognition

(defclass hmm ()
  ((belief :initarg :belief :accessor belief)
   (start-belief :initarg :start-belief :accessor start-belief)
   (state-transitions :initarg :state-transitions :accessor state-transitions)
   (observation-probabilities :initarg :observation-probabilities
                              :accessor observation-probabilities)
   (last-observation :initarg :last-observation :initform ""  :accessor last-observation)
   (observations :initarg :observations :initform () :accessor observations)))

;; Setters and getters / interface
(defun set-belief (hmm new-belief)
  (setf (belief hmm) new-belief))

(defun get-belief (hmm)
  (belief hmm))

(defun set-start-belief (hmm new-belief)
  (setf (start-belief hmm) new-belief))

(defun get-start-belief (hmm)
  (start-belief hmm))

(defun set-state-transitions (hmm new-state-transitions)
  (setf (state-transitions hmm) new-state-transitions))

(defun get-state-transitions (hmm)
  (state-transitions hmm))

(defun set-observation-probabilities (hmm new-observation-probabilities)
  (setf (observation-probabilities hmm) new-observation-probabilities))

(defun get-observation-probabilities (hmm)
  (observation-probabilities hmm))

(defun set-observations (hmm new-observations)
  (setf (observations hmm) new-observations))

(defun get-observations (hmm)
  (observations hmm))

;; NOTE: probabilities only account for single stpr. Using several stprs, normalization
;;is needed! 
(defun create-state-probabilities-from-stpr (stpr)
  "Calculates state-probabilities out of a spatio-temporal plan representation STPR by
   expectation maximization count and returns a belief as a hashtable with normalized
   probabilities for every state."
  (let ((locations-list NIL)
         (state-probabilities (make-hash-table)))
    (dolist (location (st-list stpr))
      (unless (member (location-label location) locations-list)
        (setf locations-list (cons (location-label location) locations-list))
        (setf (gethash (location-label location) state-probabilities) 0))
      (setf (gethash (location-label location) state-probabilities)
            (+ 1 (gethash (location-label location) state-probabilities))))
    (dolist (location locations-list)
      (setf (gethash location state-probabilities)
            (/ (gethash location state-probabilities) (length (st-list stpr)))))
    state-probabilities))

;; Assuming uniform distribution for single plans, we incorporate plan probabilities
(defun create-state-probabilities-from-stpr-library (stpr-library)
  "Calculates state-probabilities out of a library of spatio-temporal plan representations
   STPR-LIBRARY using a expectation maximization count and assuming uniform distribution over
   single plan probabilities."
  (let ((state-matrix (make-hash-table)) (state-name NIL)
        (number-plans (length (stpr-list stpr-library))) (sum 0))
    (format t "Creating full state probabilities for ~s plans... ~%" number-plans)
    (dolist (stpr (stpr-list stpr-library))
      (let ((state-probabilities (create-state-probabilities-from-stpr stpr)))
        (maphash #'(lambda (key value)
                     (setf state-name
                           (concatenate 'string (string (short-name stpr)) "-" (string key)))
                     (setf (gethash state-name state-matrix) (/ value number-plans))
                     (format t "--- Adding entry: ~s : ~s~%" state-name (/ value number-plans))
                     (unless (eq number-plans 0)
                       (incf sum (/ value number-plans))))
                 state-probabilities)))
    (format t "Sum of all state-probabilities is ~s~%" sum)
    state-matrix))

;; Create uniformly distributed initial state probabilities
(defun create-uniform-state-probabilities-from-stpr-library (stpr-library)
  "Calculates uniformly distributed state-probabilities over all states appearing in
   all spatio-temporal plan representations of STPR-LIBRARY"
  (let ((state-matrix (make-hash-table))
        (number-plans (length (stpr-list stpr-library))) (number-states 0))
    (format t "Creating full state probabilities for ~s plans... ~%" number-plans)
    (dolist (stpr (stpr-list stpr-library))
      (let ((state-probabilities (create-state-probabilities-from-stpr stpr)))
      (setf state-probabilities (create-state-probabilities-from-stpr stpr))
      (maphash #'(lambda (key value)
                   (declare (ignore value))
                   (let ((state-name
                          (concatenate 'string (string (short-name stpr)) "-" (string key))))
                     (setf (gethash state-name state-matrix) 0))
                   (setf number-states (+  number-states 1))) state-probabilities)))
    (maphash #'(lambda (key value)
                 (declare (ignore value))
                 (setf (gethash key state-matrix) (/ 1 number-states))
                 ;; (format t "Adding state-probability: ~s -- ~s~%"
                 ;;         key (/ 1 number-states))
                 ) state-matrix)
    state-matrix))

(defun forward-step-belief (hmm)
  "Calculates the belief distribution over the states of the HMM after incorporating
   only doing the prediction-step of the hmm."
  (let ((new-belief (make-hash-table)))
    (maphash #'(lambda (statevar prob)
                 (declare (ignore prob))
                 (let ((bel-strich (prediction-step (belief hmm) (state-transitions hmm)
                                                   statevar)))
                 (setf (gethash statevar new-belief) bel-strich))) (belief hmm))
    new-belief))

(defun bayes-filter (hmm observation)
  "Calculates the belief distribution over the states of the HMM after incorporating
   a new OBSERVATION using bayes filtering."
  (let ((new-belief (make-hash-table)))
    (maphash #'(lambda (statevar prob)
                 (declare (ignore prob))
                 (let ((bel-strich (prediction-step (belief hmm) (state-transitions hmm)
                                                    statevar)))
                   (let ((bel (measurement-update observation bel-strich
                                                  (observation-probabilities hmm) statevar)))
                     (setf (gethash statevar new-belief) bel)))) (belief hmm))
    new-belief))

(defun prediction-step (belief state-transitions state)
  "Calculates the probability for a STATE given the current BELIEF and the
   STATE-TRANSITIONS of an hmm. This is commonly referred to as the prediction-step
   of an hmm."
  (let ((bel-strich 0) (bel-strich-iterate 0))
    (maphash #'(lambda (s prob)
                 (setf bel-strich-iterate
                       (* (get-state-transition-probability s state state-transitions)
                          prob))
                 (setf bel-strich (+ bel-strich bel-strich-iterate))) belief)
    bel-strich))

(defun measurement-update (measurement bel-strich observation-probabilities state)
  "Calculate the probability for a STATE given the current probability
   BEL-STRICH of the STATE, the OBSERVATION-PROBABILITIES of the hmm and a new
   measurement. This step is commonly referred to as the measurement update of a hmm."
    ;; get rid of plan-related prefix since we assume observations to be plan independent
  (* (get-observation-probability state measurement observation-probabilities) bel-strich))

(defun normalize-belief (belief)
  "Normalizes the posterior probabilities of an hmm after the measurement update according to
   Thrun et al. (Probabilistic Robotics book)"
  (let ((normalizer 0) (new-belief (make-hash-table)))
    (maphash #'(lambda (state probability)
                 (declare (ignore state))
                 (setf normalizer (+ normalizer probability))) belief)
    (maphash #'(lambda (state probability)
                 (setf (gethash state new-belief) (float (/ probability normalizer)))) belief)
    new-belief))

;; Belief has short-plan-forms as keys! TODO: Change in future
(defun calculate-plan-probabilities (belief)
  "Calculates probabilities for every internal state of an hierarchical hmm and returns it as
   a probability distribution BELIEF in a hashtable"
  (let ((plan-probs (make-hash-table :test 'equalp)) (plan NIL))
    (maphash #'(lambda (state prob)
                 ;; Use plan prefix for identification of internal state
                 (setf plan (subseq state 0 (position #\- state)))
                 (if (gethash (string (get-full-plan-name plan)) plan-probs)
                   (setf (gethash (string (get-full-plan-name plan)) plan-probs)
                         (float (+ (gethash (string (get-full-plan-name plan)) plan-probs) prob)))
                   (setf (gethash (string (get-full-plan-name plan)) plan-probs)
                         prob))) belief)
    plan-probs))

(defun add-observation-to-hmm (motion-tracking-data objects-cache full-spatial-model duration hmm)
  "Adds an observation of the motion tracker data DATA and object recognitions OBJECTS-CACHE to the
   HMM and calculates a new belief distribution. FULL-SPATIAL-MODEL defines the locations as a set
   of locations from our knowledge-base, LAST-STOP-TIME and ROS-TIME are used to calculate durations."
  (let ((location-observation "") (new-belief (make-hash-table)))
    (setf location-observation  (string (label (get-most-likely-gaussian motion-tracking-data full-spatial-model))))
    ;; Append observation to end of list
    (setf (observations hmm) (reverse (cons location-observation (reverse (observations hmm)))))
    
    ;; (format t "~% >>> Saw human standing for ~3$ s at ~s with object ~s)~%"
    ;;         duration location-observation objects-cache)
    
    ;; Here I apply a filtering for same locations that are not supposed to appear several
    ;; times after each other acounting for bad location recognition. TODO: This should actually
    ;; be accounted for in hhmm and by improving location recognition
    (let  ((new-belief
            (if (string= location-observation (last-observation hmm))
              (setf new-belief (belief hmm))
              (progn
                (format t "~% >>> Saw human standing for ~3$ s at ~s with object ~s)~%"
                        duration location-observation objects-cache)
                (setf new-belief ;; (normalize-belief
                      ;;  (bayes-filter hmm location-observation))
                      (normalize-belief
                       (forward-backward hmm)))))))
      (setf (last-observation hmm) location-observation)
      (setf (belief hmm) new-belief))
    new-belief))

(defun forward-backward (hmm)
  "Forward-Backward algorithm to calculate posterior marginals for all hidden state variables in BELIEF given
   a sequence of OBSERVATIONS"
  (let ((fwd ()) (i 0) (j 0) (f-curr NIL) (prev-f-sum NIL) (f-prev NIL)
        (p-fwd 0) 
        (b-curr NIL) (b-curr-sum 0) (b-prev (make-hash-table)) (bkw ())
        (posterior (make-hash-table)) (end-state "E"))
    ;; (format t ">>>>>>>> Forward-Backward Starting...~%")
    ;; Iterate through observations
    (setf fwd (cons (start-belief hmm) fwd))
    (dolist (observation (observations hmm))
      (setf f-curr (make-hash-table))
      (maphash #'(lambda (st prob)
                   (if (eq i 0)
                     (setf prev-f-sum prob)
                     ;; If not first step, use transition probs to get state prob.
                     (progn
                       (setf prev-f-sum 0)
                       (maphash #'(lambda (state prob)
                                    (declare (ignore prob))
                                    ;; Create state transition string
                                    (setf prev-f-sum
                                          (+ prev-f-sum (*
                                                         (gethash (string state) f-prev)
                                                         (get-state-transition-probability
                                                          state st
                                                          (state-transitions hmm))))))
                                (start-belief hmm)))) ;; end if
                   (setf (gethash st f-curr)
                         (*
                          prev-f-sum
                          (get-observation-probability
                           st observation
                           (observation-probabilities hmm)))))
               (start-belief hmm))
      ;; We add elements to the front of the list, => has to be reversed later!!!
      (setf fwd (cons (normalize-belief f-curr) fwd))
      (setf f-prev f-curr)
      (setf i (+ i 1))
      ;; (format t "~%>>>> STEP ~s >>>> Forward: f-curr is:~%" i)
      ;; (maphash #'print-hash-entry (normalize-belief f-curr))
      )
    ;; Reverse list due to cons before
    (setf fwd (reverse fwd))
    (maphash #'(lambda (k prob)
                 (declare (ignore prob))   
                 (setf p-fwd
                       (+ p-fwd
                          (*
                           (gethash k f-curr)
                           (get-state-transition-probability k end-state (state-transitions hmm))))))
             (start-belief hmm))
    ;; Backward step
    (dolist (observation (cons (string 'None) (reverse (observations hmm))))
      (setf b-curr (make-hash-table))
      (maphash #'(lambda (st prob)
                   (declare (ignore prob))
                   (if (eq j 0) 
                     (progn
                       (setf (gethash st b-curr)
                             (get-state-transition-probability
                              st end-state
                              (state-transitions hmm))))
                     (progn
                       (setf (gethash st b-curr) 0)
                       (maphash #'(lambda (l prob)
                                    (declare (ignore prob))
                                    (setf b-curr-sum 0)
                                    (setf
                                     (gethash st b-curr)
                                     (+ (gethash st b-curr)
                                        (*
                                         (get-state-transition-probability st l (state-transitions hmm))
                                         (get-observation-probability l observation (observation-probabilities hmm))
                                         (gethash l b-prev)))))
                                (start-belief hmm)))))
               (start-belief hmm))
      (setf bkw (cons (normalize-belief b-curr) bkw))
      (setf b-prev b-curr)
      (setf j (+ j 1))
      ;; (format t "~%>>>> STEP ~s >>>> Forward: b-curr is:~%" j)
      ;; (maphash #'print-hash-entry (normalize-belief b-curr))
      )
      
    ;; (format t ">>>> Backward step done...~%")
    ;; Calculate posterior marginals
    (maphash #'(lambda (st prob)
                 (declare (ignore prob))
                 (let ((seq-list (create-seq-list (+ 1 (length (observations hmm))))))
                   (dolist (i seq-list)
                     (setf (gethash st posterior)
                           (/
                            (*
                             (gethash st (nth i fwd))
                             (gethash st (nth i bkw)))
                            p-fwd)))))
             (start-belief hmm))
    ;; (format t ">>>> >>>> Posterior marginals are:~%")
    ;; (maphash #'print-hash-entry (calculate-plan-probabilities (normalize-belief posterior)))
    (setf (belief hmm) (normalize-belief posterior))
    ;; (format t ">>>>>>>> Forward-Backward Finished.")
    posterior))

(defun viterbi (hmm)
  (let ( (x NIL)
         (number-states (- (hash-table-size (belief hmm)) 1))
         (number-observations (list-length (observations hmm)))
         (V NIL) (path NIL) (y-index 0) (tt 0) (max1 0) (y0-index 0)
         (prob 0) (best-path-number 0) (newpath NIL) (max-state-index 0)
         (max-state NIL) (path-prob) (best-path-prob 0))

    (format t "States: ~s, Observations:~s~%"
            (hash-table-size (belief hmm))
            (list-length (observations hmm)))
     (setf V (make-array (list number-observations number-states)))
     (setf path (make-list number-states))
     (setf newpath (make-list number-states))
     ;; First observation loop (for y in states: )
     (maphash #'(lambda  (y p)
                  (declare (ignore p))
                  (setf (aref V tt y-index)
                        (*
                         (gethash y (start-belief hmm))
                         (get-observation-probability
                          y (first (observations hmm))
                          (observation-probabilities hmm))))
                  (setf (nth y-index path) (list y))
                  (incf y-index 1))
             (start-belief hmm))
     (format t "------------------------ 0. Observation: 0 - ~s -----------------~%"
             (first (observations hmm)))
     ;; Iterate through observations without the first one!
     (incf tt 1)
     (dolist (observation (rest (observations hmm)))
       (format t "----------------------- ~s. Observation: ~s~%" tt observation)
       (setf y-index 0)
       (maphash #'(lambda (state-y prob-y)
                    (declare (ignore prob-y))
                    (setf max1 0)
                    (setf y0-index 0)
                    (maphash #'(lambda (state-y0 prob-y0)
                                 (declare (ignore prob-y0))
                                 (setf prob
                                       (* (aref V (- tt 1) y0-index)
                                          (get-state-transition-probability
                                           state-y0 state-y
                                           (state-transitions hmm))
                                          (get-observation-probability
                                           state-y
                                           observation
                                           (observation-probabilities hmm))))
                                 (when (>= prob max1)
                                    (setf max1 prob)
                                    (setf max-state state-y)
                                    (setf max-state-index y0-index))
                                 (incf y0-index 1))
                             (belief hmm))
                    (setf (aref V tt y-index) max1)
                    (setf (nth y-index newpath)
                          (append (nth max-state-index path)
                                  (list max-state)))
                    (incf y-index 1))
                (belief hmm))
       ;; Dont need to remember old paths
       (setf path newpath)
       (setf newpath (make-list number-states))
       (incf tt 1)) ;; observations-loop end
     ;; (format t "~s~%" V)
     ;; Search for path with highes likelihood in V
     (loop for x2 from 0 to (- number-states 1) do
          (setf path-prob (aref V (- number-observations 1) x2))
          (when (> path-prob best-path-prob)
            (setf best-path-number x2)
            (setf best-path-prob path-prob)
            (setf x (nth x2 path))))
     x))

(defun estimate-emission-probs-grid ()
  "This function estimated observation probabilities for the HHMM from the spatial
   model. NOTE: This highly depends on the paramaters since the DELTA values define
   the area which is to be searched. For now it is +-3 times the variance"
  (let ((stpr-lib (generate-stpr-library))
        (str-loc-tab NIL)
        (spatial-model NIL)
        (gaussian NIL)
        (resolution 0.01) ;; was 0.01
        (delta-x NIL)
        (delta-y NIL)
        (most-likely-gaussian NIL)
        (hashtag NIL)
        (probs (make-hash-table :test 'equalp)))
    (format t "Estimating HHMM observation probs from spatial model~%")
    (setf str-loc-tab (generate-location-string-table stpr-lib))
    (setf spatial-model (create-full-spatial-model stpr-lib str-loc-tab))
    
    (dolist (labelled-gaussian (labelled-gaussians spatial-model))
            (format t "-- Processing gaussian: ~s~%" (label labelled-gaussian))
            (setf gaussian (gaussian labelled-gaussian))
            (format t "--- Mean: ~s,~s, var: ~s,~s~%"
                    (mean-x (gaussian labelled-gaussian))
                    (mean-y (gaussian labelled-gaussian))
                    (var-x (gaussian labelled-gaussian))
                    (var-y (gaussian labelled-gaussian)))
            
            (setf delta-x (* 3 (var-x gaussian)))
            (setf delta-y (* 3 (var-y gaussian)))
            (format t "--- Generating Grid: x -- [~s,~s], y -- [~s,~s]~%"
                    (- (mean-x gaussian) delta-x)
                    (+ (mean-x gaussian) delta-x)
                    (- (mean-y gaussian) delta-y)
                    (+ (mean-y gaussian) delta-y))
            ;; Create grid around mean
            (do ((y (- (mean-y gaussian) delta-y) (+ y resolution)))
                ((> y (+ (mean-y gaussian) delta-y)))
              (do ((x (- (mean-x gaussian) delta-x) (+ x resolution)))
                  ((> x (+ (mean-x gaussian) delta-x)))
                (setf most-likely-gaussian
                      (label (get-most-likely-gaussian-point x y spatial-model)))
                (setf hashtag (concatenate 'string
                                           (string (label labelled-gaussian))
                                           "-"
                                           (string most-likely-gaussian)))
                
                (if (eq (gethash hashtag probs) NIL)
                  (setf (gethash hashtag probs) 1)
                  (setf (gethash hashtag probs) (+
                                                 (gethash hashtag probs)
                                                 1))))))
    ;; Now normalize using number of samples
    (setf probs (normalize-by-locations probs))
    (format t "~% ~% Estimated Observation Probabilities:~%")
    (maphash #'print-hash-entry probs)
    probs))

(defun estimate-emission-probs-probdist ()
  "This function estimated observation probabilities for the HHMM from the spatial
   model. This implementation draws samples randomly over a grid defined by AREA-X
   and AREA-Y and randomly draws these samples from the gaussian distribution that
   belongs to the current locations"
  (let ((stpr-lib (generate-stpr-library))
        (str-loc-tab NIL)
        (spatial-model NIL)
        (gaussian NIL)
        (gauss-prob NIL)
        (random-x NIL)
        (random-y NIL)
        (sample-area-x 5000) ;;define area to sample in mm
        (sample-area-y 5000)
        (number-samples 5000)
        (i 0)
        (random-number NIL)
        (most-likely-gaussian NIL)
        (hashtag NIL)
        (probs (make-hash-table :test 'equalp)))
    (format t "Estimating HHMM observation probs from spatial model drawing samples from gaussian dist~%")
    (setf str-loc-tab (generate-location-string-table stpr-lib))
    (setf spatial-model (create-full-spatial-model stpr-lib str-loc-tab))
    (dolist (labelled-gaussian (labelled-gaussians spatial-model))
            (format t "-- Processing gaussian: ~s~%" (label labelled-gaussian))
            (setf gaussian (gaussian labelled-gaussian))
            (format t "--- Mean: ~s,~s, var: ~s,~s~%"
                    (mean-x (gaussian labelled-gaussian))
                    (mean-y (gaussian labelled-gaussian))
                    (var-x (gaussian labelled-gaussian))
                    (var-y (gaussian labelled-gaussian)))
            (setf i 0)
            (loop while (< i number-samples) do
                 (setf random-x (/ (random sample-area-x) 1000))
                 (setf random-y (/ (random sample-area-y) 1000))
                 (setf gauss-prob (inside-gaussian random-x random-y
                                                   (gaussian labelled-gaussian)))
                 (setf random-number (random 100))
                 (when (< random-number (* 100 gauss-prob))
                   (setf most-likely-gaussian
                          (label
                           (get-most-likely-gaussian-point random-x random-y spatial-model)))
                   (setf hashtag (concatenate 'string
                                               (string (label labelled-gaussian))
                                               "-"
                                               (string most-likely-gaussian)))
                   (if (eq (gethash hashtag probs) NIL)
                      (setf (gethash hashtag probs) 1)
                      (setf (gethash hashtag probs) (+
                                                     (gethash hashtag probs)
                                                     1)))
                   (incf i 1))))
    ;; Now normalize using number of samples
    (setf probs (normalize-by-samples probs number-samples))
    (format t "~% ~% Estimated Observation Probabilities:~%")
    (maphash #'print-hash-entry probs)
    probs))

(defun normalize-by-samples (probabilities number-samples)
  "Normalizes estimated transition probabilities based on the number of samples. Given
   the number of samples for each state is known, we can just divide each number of
   instances by the number of samples"
  (let ((new-probabilities (make-hash-table :test 'equalp)))
    (maphash #'(lambda (state counter)
                 (setf (gethash state new-probabilities)
                       (float (/ counter number-samples)))
             ) probabilities)
    new-probabilities))

(defun normalize-by-locations (probabilities)
  "Normalizes estimated transition probabilities for each state. Therefore it simply
   sums up all instances of each state and divides each instance by the sum for each
   corresponding state."
  (let ((new-probabilities (make-hash-table :test 'equalp))
         (sum-table (make-hash-table :test 'equalp))
        (location NIL))
    ;; First generate list of locations
    (maphash #'(lambda (state counter)
                 (setf location (subseq state 0 (position #\- state)))
                 (if (eq (gethash location sum-table) NIL)
                   (setf (gethash location sum-table) counter)
                   (setf (gethash location sum-table)
                                  (+ (gethash location sum-table) counter))))
             probabilities)
    (format t "Table of sums:~%")
    (maphash #'print-hash-entry sum-table)

    (maphash #'(lambda (state counter)
                 (setf location (subseq state 0 (position #\- state)))
                 (setf (gethash state new-probabilities)
                       (float (/ counter (gethash location sum-table)))))
             probabilities)
    new-probabilities))