(in-package :ad-exe)

;; helper functions for plan recognition and monitoring

;; Calculate square root
(defun square (x) (* x x))

;; Calculate Levenshtein-distance between two strings
(defun levenshtein-distance (str1 str2)
  "Calculates the Levenshtein distance between str1 and str2, returns an editing distance (int)."
  (let ((n (length str1))
        (m (length str2)))
    ;; Check trivial cases
    (cond ((= 0 n) (return-from levenshtein-distance m))
          ((= 0 m) (return-from levenshtein-distance n)))
    (let ((col (make-array (1+ m) :element-type 'integer))
          (prev-col (make-array (1+ m) :element-type 'integer)))
      ;; We need to store only two columns---the current one that
      ;; is being built and the previous one
      (dotimes (i (1+ m))
        (setf (svref prev-col i) i))
      ;; Loop across all chars of each string
      (dotimes (i n)
        (setf (svref col 0) (1+ i))
        (dotimes (j m)
          (setf (svref col (1+ j))
                (min (1+ (svref col j))
                     (1+ (svref prev-col (1+ j)))
                     (+ (svref prev-col j)
                        (if (char-equal (schar str1 i) (schar str2 j)) 0 1)))))
        (rotatef col prev-col))
      (svref prev-col m))))

(defun levenshtein-similarity (str1 str2)
  "Calculates Generalized Levenshtein similarity between strings STR1 and STR2."
  (/ (- (+ (length str1) (length str2)) (levenshtein-distance str1 str2))
     (+ (length str1) (length str2)))
  )

;; This function assigns a unique string to every unique location
(defun generate-location-string-table (stpr-library)
  (let ((loc-str-table (make-hash-table))
         (counter 0)
        (abc '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)))
  (dolist (stpr (stpr-list stpr-library))
    (dolist (timed-location (st-list stpr))
      (unless (gethash (location-label timed-location) loc-str-table)
        (format t "Assigning string ~s to location ~s~%"
                (string (nth counter abc)) (location-label timed-location))
        (setf (gethash (location-label timed-location) loc-str-table)
              (string (nth counter abc)))
        (setf counter (+ 1 counter)))))
  (return-from generate-location-string-table loc-str-table)))

;; Calculate the value of a 2D coordinate in a gaussian
(defun inside-gaussian (x y gaussian)
  "Calculates probability of point X,Y beeing in GAUSSIAN."
  (exp (* -1
          (+
           (/ (square (- x (mean-x gaussian)))
              (* 2 (var-x gaussian)))
           (/ (square (- y (mean-y gaussian)))
              (* 2 (var-y gaussian)))))))

(defun 1d-gauss-value (mean var x)
  (*
   (/ 1
      (* var (sqrt (* 2 pi))))
   (exp
    (* (* -1 0.5)
       (square (/ (- x mean) var))))))

(defun normalized-1d-gauss (mean var x)
  (/
   (1d-gauss-value mean var x)
   (1d-gauss-value mean var mean)))

;; TODO: For now, an object is in reach if it is with a radius of 0.5 m away from the
;; human. This should be checked/learned
(defun in-reach (x-human y-human  x-object y-object)
  "Checks if an object with coordinates X-OBJECT,Y-OBJECT is in reach of a human that
   is at X-HUMAN,Y-HUMAN."
  (if (< (cl-transforms:v-dist (cl-transforms:make-3d-vector x-human y-human 0)
                            (cl-transforms:make-3d-vector x-object y-object 0)) 0.5)
    T
    NIL))

(defun print-hash-entry (key value)
  "Helper funtion for printing hashtables."
    (format t "~S --- ~S~%" key value))

(defun get-full-plan-name (short-name)
  "Defines the mapping between full plan names and their shortcuts (HARDCODED!)."
  (let ((plan-mapping (make-hash-table :test 'equalp)))
    (setf (gethash (string 'DW) plan-mapping) 'PREPARE-DRINK-PLAN)
    (setf (gethash (string 'STC) plan-mapping) 'PREPARE-CORNFLAKES-PLAN)
    (setf (gethash (string 'STQ) plan-mapping) 'PREPARE-QUARK-PLAN)
    (setf (gethash (string 'CTC) plan-mapping) 'CLEAN-TABLE-CORNFLAKES-PLAN)
    (setf (gethash (string 'CTQ) plan-mapping) 'CLEAN-TABLE-QUARK-PLAN)
    (setf (gethash (string 'ST) plan-mapping) 'PREPARE-TABLE-PLAN)
    (setf (gethash (string 'CT) plan-mapping) 'CLEAN-TABLE-PLAN)
    (setf (gethash (string 'PW) plan-mapping) 'PREPARE-FOR-WORK-PLAN)
    (return-from get-full-plan-name (gethash (string short-name) plan-mapping))))

(defun get-short-plan-name (full-name)
  "Defines the mapping between plan shortcuts names and their full names(HARDCODED!)."
  (let ((plan-mapping (make-hash-table :test 'equalp)))
    (setf (gethash (string 'PREPARE-DRINK-PLAN) plan-mapping) 'DW)
    (setf (gethash (string 'PREPARE-CORNFLAKES-PLAN) plan-mapping) 'STC)
    (setf (gethash (string 'PREPARE-QUARK-PLAN) plan-mapping) 'STQ)
    (setf (gethash (string 'CLEAN-TABLE-CORNFLAKES-PLAN) plan-mapping) 'CTC)
    (setf (gethash (string 'CLEAN-TABLE-QUARK-PLAN) plan-mapping) 'CTQ)
    (setf (gethash (string 'PREPARE-TABLE-PLAN) plan-mapping) 'ST)
    (setf (gethash (string 'CLEAN-TABLE-PLAN) plan-mapping) 'CT)
    (setf (gethash (string 'PREPARE-FOR-WORK-PLAN) plan-mapping) 'PW)
    (return-from get-short-plan-name (gethash (string full-name) plan-mapping))))

(defun visualize-plan-probs (plan-probs)
  "Visualization for plan probabilities of HHMM (to be used with rxplot)."
    (maphash #'(lambda (plan prob)
                 (let ((pub (roslisp:advertise
                             (concatenate 'string "HMM-" (string plan))
                             "std_msgs/Float32")))
                   (roslisp:publish-msg pub :data (* 100 prob))))
             plan-probs))

(defun visualize-normality (normality)
  "Visualization for average normality (to be used with rxplot)."
  (let ((pub (roslisp:advertise
              "avg-normality"
              "std_msgs/Float32")))
    (unless (null normality)
      (roslisp:publish-msg pub :data normality))))

(defun visualize-particleset (particleset plan-library)
  "Visualization of plan probabilities using Particle filter (for rxplot)"
  (let ((plans-hashtable (make-hash-table)))
    (format t "_____________________________________________________________________________~%")
    ;; generate hashtable from all plans
    (dolist (plan (stpr-list plan-library))
      (setf (gethash (string (name plan)) plans-hashtable) 0))
    (dolist (particle particleset)
      (setf (gethash (string (name (model-stpr particle))) plans-hashtable)
            (+ 1 (gethash (string (name (model-stpr particle))) plans-hashtable))))
    (dolist (plan (stpr-list plan-library))
      (format t "# Particles of ~30@A: ~5A         (~1$ %)~%"
              (string (name plan))
              (gethash (string (name plan)) plans-hashtable)
              (* 100 (/ (gethash (string (name plan)) plans-hashtable) (length particleset))))
      ;; Also post probability for every plan as ROS topic to plot with rxplot
      (let ((pub (roslisp:advertise (string (name plan)) "std_msgs/Float32")))
        (roslisp:publish-msg pub :data
                             (* 100 (/ (gethash (string (name plan)) plans-hashtable)
                                       (length particleset))))))
    (format t "_____________________________________________________________________________~%")))

(defun avg (weights-list)
  "Calculates the average of the list of numbers in WEIGHTS-LIST."
  (let ((counter 0) (sum 0))
    (dolist (weight weights-list)
      (setf sum (+ sum weight))
      (setf counter (+ 1 counter)))
    (/ sum counter)))

(defun product (weights-list)
  "Calculates the product of the list of weights WEIGHTS-LIST"
  (let ((prod 1))
    (dolist (weight weights-list)
      (setf prod (* weight prod)))
    (return-from product prod)))

(defun create-string-from-stpr (stpr)
  "Creates a string representation for the patterns of locations of an STPR"
  (let ((model-string ""))
    (dolist (timed-location (st-list stpr))
      (setf model-string
            (concatenate 'string
                         (object-string-label timed-location)
                         model-string)))
      (return-from create-string-from-stpr model-string)))

(defun create-string-without-doubles (observations)
  "Takes a string OBSERVATIONS and eliminates multiple appearances of characters
   that appear directly after each other."
  (let ((last-observation-string "") (observation-string ""))
    (dolist (timed-location observations)
      (unless (string= last-observation-string (object-string-label timed-location))
        (setf observation-string
              (concatenate 'string observation-string
                           (object-string-label timed-location))))
      (setf last-observation-string (object-string-label timed-location)))
    (return-from create-string-without-doubles observation-string)))

(defun gaussian->points (gaussian prob &key (resolution 0.02) (gridsize 7))
  "Samples a gaussian into a list of points."
  (let ((points-list NIL))
    (setf resolution (/ resolution prob))
    (do ((y (- (mean-y gaussian) (* gridsize (var-y gaussian))) (+ y resolution)))
        ((> y (+ (mean-y gaussian) (* gridsize (var-y gaussian)))))
      (do ((x (- (mean-x gaussian) (* gridsize (var-x gaussian))) (+ x resolution)))
          ((> x (+ (mean-x gaussian) (* gridsize (var-x gaussian)))))
        ;; (format t "point:  [~s, ~s ~s]~%" x y (inside-gaussian x y gaussian))
        (setf points-list
              (cons (cl-transforms:make-3d-vector
                     x
                     y
                     (inside-gaussian x y gaussian)) points-list))))
    (return-from gaussian->points points-list)))

(defun 3d-vector->oriented-bounding-box (vector &key (resolution 0.05))
  "Converts a 3d-vector to an oriented bounding box."
  (declare (type cl-transforms:3d-vector vector))
  (declare (type number resolution))
  (roslisp:make-msg "arm_navigation_msgs/OrientedBoundingBox"
                    (x center) (tf:x vector)
                    (y center) (tf:y vector)
                    (z center) (tf:z vector)
                    (x extents) resolution
                    (y extents) resolution
                    (z extents) resolution
                    (x axis) 1))

(defun points->collision-map (points &key (frame-id "map"))
  (declare (type list points))
  "Converts a list of TF:3D-VECTOR to an arm_navigation_msgs/CollisionMap message."
  (roslisp:make-msg "arm_navigation_msgs/CollisionMap"
            (frame_id header) frame-id
            boxes (map 'vector #'3d-vector->oriented-bounding-box points)))


(defun get-plan-probabilities (particleset plan-library)
  "Returns a hastable with probability values for different plans given a PARTICLESET of
   the particle-filter and a PLAN-LIBRARY"
  (let ((plans-hashtable (make-hash-table :test 'equalp)) (best-plan-count 0) (best-plan NIL))
    (dolist (plan (stpr-list plan-library))
      (setf (gethash (string (name plan)) plans-hashtable) 0))
    (dolist (particle particleset)
      (setf (gethash (string (name (model-stpr particle))) plans-hashtable)
            (+ 1 (gethash (string (name (model-stpr particle))) plans-hashtable))))
    (dolist (plan (stpr-list plan-library))
      (setf
       (gethash (string (name plan)) plans-hashtable)
       (float (/ (gethash (string (name plan)) plans-hashtable)
                 (length particleset))) )
      ;; (format t "Plan ~s: ~3$~%"
      ;;         (string (name plan))
      ;;         (gethash (string (name plan)) plans-hashtable) )
      (when (> (gethash (string (name plan)) plans-hashtable) best-plan-count)
        (setf best-plan (string (name plan)))
        (setf best-plan-count (gethash (string (name plan)) plans-hashtable))))

    (return-from get-plan-probabilities plans-hashtable)))

(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos)))

(defun return-orientation-yaw (pose)
  "Takes an POSE as nav_msgs/Odometry as input and returns the
   yaw of the orientation"
    (cl-transforms:get-yaw (cl-transforms:make-quaternion
                          (geometry_msgs-msg:x
                           (geometry_msgs-msg:orientation
                            (geometry_msgs-msg:pose
                             (nav_msgs-msg:pose pose))))
                          (geometry_msgs-msg:y
                           (geometry_msgs-msg:orientation
                            (geometry_msgs-msg:pose
                             (nav_msgs-msg:pose pose))))
                          (geometry_msgs-msg:z
                           (geometry_msgs-msg:orientation
                            (geometry_msgs-msg:pose
                             (nav_msgs-msg:pose pose)))) 
                          (geometry_msgs-msg:w
                           (geometry_msgs-msg:orientation
                            (geometry_msgs-msg:pose
                             (nav_msgs-msg:pose pose)))))))

(defun merge-beliefs (belief-1 belief-2)
  "Merges hashtables BELIEF-1 and BELIEF-2 by comparing the
   keys and calculating averages of the values. Returns merged
   hashtable MERGED-BELIEF"
  (let ((merged-belief (make-hash-table :test 'equalp)))
  (maphash #'(lambda (plan prob)
               (setf (gethash (string plan) merged-belief)
                     (avg
                      (list (float (gethash (string plan) belief-2))
                            (float prob)))))
                      belief-1)
  merged-belief))

(defun penalyze-beliefs (belief penalty-belief)
  "Takes binary PENALTY-BELIEF and penalyzes if PROB in PENALTY-BELIEF == 0
   by setting PROB in MERGED-BELIEF to avg(0,prob). If PENALTY != 0, leave old
   PROB"
  (let ((merged-belief (make-hash-table :test 'equalp)))
  (maphash #'(lambda (plan prob)
               (if (eq (gethash (string plan) penalty-belief) 0)
                 ;; Penalyze when prob = 0
                 (setf (gethash (string plan) merged-belief)
                       (avg (list (float 0)
                                  (float prob))))
                 (setf (gethash (string plan) merged-belief) prob)))
           belief)
  merged-belief))

(defun weight-beliefs (belief weight-belief)
  "Weights hashtable BELIEF according to weights given in
   hastable weight-belief. Returns merged
   hashtable MERGED-BELIEF"
  (let ((merged-belief (make-hash-table :test 'equalp)))
  (maphash #'(lambda (plan prob)
               (if (eq prob 0)
                   (setf (gethash (string plan) merged-belief)
                         (*
                          0.001
                          (float (gethash (string plan) belief))))
                   (setf (gethash (string plan) merged-belief)
                         (*
                          (float (gethash (string plan) belief))
                          (float prob)))))
           weight-belief)
  (normalize-belief merged-belief)))

(defun bias-belief-with-plan-probs (belief plan-probs)
  "Weights a BELIEF consisting about substates of plans according
   to the probabilities in PLAN-PROBS"
  (let ((new-belief (make-hash-table :test 'equalp))
         (state-plan NIL)
        (belief-sum 0))
    ;; Iterate through plan probs
    (maphash #'(lambda (plan prob1)
                 ;; Iterate through states and bias them
                 (maphash #'(lambda (state prob2)
                              (setf state-plan  (subseq (string state) 0 (position #\- state)))
                              (when (string= state-plan (string (get-short-plan-name plan)))
                                (setf (gethash (string state) new-belief)
                                      (* prob1 prob2))
                                (setf belief-sum (+ (* prob1 prob2) belief-sum))))
                          belief ))
             plan-probs)
    (format t "Belief sum is: ~s~%" belief-sum)
    new-belief))

(defun create-seq-list (n)
  "Creates a list with entries from 1 to N"
  (loop for i from 0 upto (- n 1) collect i))

(defun write-plan-probs-to-csv (plan-probs filename)
  "Writes a csv-file of plan probabilities in the hashtable PLAN-PROBS
   for visualization with gnuplot."
  (let ((i 0))
    (with-open-file (file filename
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
      ;;Write csv header
      (maphash #'(lambda (plan prob)
                   (declare (ignore plan))
                   (format file "~s" prob)
                   (format file ",")
                   (incf i 1))
               plan-probs)
      (format file "~s~%" (roslisp:ros-time)))))

(defun write-average-normality (filename normality)
  "Writes a csv-file of normality values and the time"
  (with-open-file (file filename
                        :direction :output
                        :if-exists :append
                        :if-does-not-exist :create)
  (format file "~s,~s~%" (roslisp:ros-time) normality)))

(defun write-loc-probs-to-csv (belief filename)
  "Writes a csv-file of location probabilities in the hashtable BELIEF
   for visualization with gnuplot."
  (let ((plan-independent-state "") (loc-probs (make-hash-table :test 'equalp)))
    (with-open-file (file filename
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
      (maphash #'(lambda (state prob)
                   (setf plan-independent-state
                         (string (subseq state (+ 1 (position #\- state)))))
                   (if (gethash plan-independent-state loc-probs)
                     (setf (gethash plan-independent-state loc-probs)
                           (+ (gethash plan-independent-state loc-probs) prob))
                     (setf (gethash plan-independent-state loc-probs) prob)))
               belief)
      ;; (format t "Location probs:~%")
      ;; (maphash #'print-hash-entry loc-probs)
      ;;Write csv
      (maphash #'(lambda (loc prob)
                   (declare (ignore loc))
                   (format file "~s" prob)
                   (format file ","))
               loc-probs)
      (format file "~s~%" (roslisp:ros-time)))))

(defun merge-loc-probs (belief)
  "Merged plan-dependent states by marginalizing out plans from conditional
   probability distribution BELIEF as explained in RSS-HRC paper. Returns
   probability-dist over internal states of HHMM as hashtable "
  (let ((plan-independent-state "") (loc-probs (make-hash-table :test 'equalp)))
    (maphash #'(lambda (state prob)
                 (setf plan-independent-state
                       (string (subseq state (+ 1 (position #\- state)))))
                 (if (gethash plan-independent-state loc-probs)
                   (setf (gethash plan-independent-state loc-probs)
                         (+ (gethash plan-independent-state loc-probs) prob))
                   (setf (gethash plan-independent-state loc-probs) prob)))
             belief)
    loc-probs))

(defun write-good-plan-obs-to-csv (good-plan-observations filename)
  "Writes the good plan observations from the hashtable GOOD-PLAN-OBERSVATIONS
   to the csv-file FILENAME"
  (with-open-file (file filename
                        :direction :output
                        :if-exists :append
                        :if-does-not-exist :create)
    (format file "~s," (roslisp:ros-time))
    (maphash #'(lambda (plan observations)
                 (declare (ignore plan))
                 (format file "~s," observations))
             good-plan-observations)
    (format file "0~%")))

(defun write-normality-tree-to-csv (normality-tree filename)
  "Writes the NORMALITY-TREE to the csv-file FILENAME.
   NOTE: This funtion is a dirt hack made due to lack of time
   => REWRITE IT TO GET MORE GENERAL"
  (with-open-file (file filename
                        :direction :output
                        :if-exists :append
                        :if-does-not-exist :create)
    (format file "~s," (roslisp:ros-time))
    (dolist (normality normality-tree)
      (format file "~s, ~s~%" (average (get-rid-of-NILs normality)) normality))))

(defun distance (pose-data last-pose-data)
  "Calculates 2d-distance between the two nav_msgs/Odometry POSE-DATA
   and the geometry_msgs/Point LAST-POSE-DATA
   TODO: Generalize to both use Odometry MSG!!!!"
  (cl-transforms:v-dist (cl-transforms:make-3d-vector
                         (geometry_msgs-msg:x
                          (geometry_msgs-msg:position
                           (geometry_msgs-msg:pose
                            (nav_msgs-msg:pose pose-data))))
                         (geometry_msgs-msg:y
                          (geometry_msgs-msg:position
                           (geometry_msgs-msg:pose
                            (nav_msgs-msg:pose pose-data))))
                         0)
                        (cl-transforms:make-3d-vector
                         (geometry_msgs-msg:x last-pose-data)
                         (geometry_msgs-msg:y last-pose-data)
                         0)))


(defun get-2d-x (pose-data)
  "Returns the x-coordinate of a navs_msgs/Odometry message"
    (geometry_msgs-msg:x
     (geometry_msgs-msg:position
      (geometry_msgs-msg:pose (nav_msgs-msg:pose pose-data)))))

(defun get-2d-y (pose-data)
  "Returns the x-coordinate of a navs_msgs/Odometry message"
    (geometry_msgs-msg:y
     (geometry_msgs-msg:position
      (geometry_msgs-msg:pose (nav_msgs-msg:pose pose-data)))))

(defun get-max-durations-from-stpr-lib (stpr-library)
  "Traverses stpr-libary and generates hashtable including maximum duration for
   each location found in STPRs"
  (let ((max-durations-table (make-hash-table :test 'equalp)))
    (dolist (stpr (stpr-list stpr-library))
      (dolist (location (st-list stpr))
        (if (eq (gethash (string (location-label location)) max-durations-table) NIL)
          (setf (gethash (string (location-label location)) max-durations-table)
                (float (duration location)))
          (when (> (duration location)
                   (gethash (string (location-label location)) max-durations-table)) 
            (setf (gethash (string (location-label location)) max-durations-table)
              (float (duration location)))))))
    max-durations-table))