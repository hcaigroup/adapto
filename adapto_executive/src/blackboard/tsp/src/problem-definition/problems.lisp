(in-package "TSP")

; --------------------------------------------------------------------- ;
; -- DEFINITION OF PROBLEMS ------------------------------------------- ;
; --------------------------------------------------------------------- ;

(defclass problem ()
  ( (name :initform nil :initarg :name :accessor name)
    (start-point :initform nil :initarg :start-point :accessor start-point)
    (end-point :initform nil :initarg :end-point :accessor end-point)
    (other-points :initform () :initarg :other-points :accessor other-points)
    (dimension :initform 0 :initarg :dimension :accessor dimension)
    (variant :initform :tsp :initarg :variant :accessor variant)
    (solution :initform () :initarg :solution :accessor solution) ))

; ---------------------------------------------;
; -- GENERAL OPERATIONS ON PROBLEMS -----------;
; ---------------------------------------------;

;(defun build-problem-solution (problem solution-list)
;  (setf (solution problem)
;        (cons (end-point problem) solution-list))
;  problem)

(defun evaluate-problem-solution (problem)
  (when (solution problem)
    (evaluate-path (solution problem))))

(defun get-problem-points (problem)
  (cons (start-point problem) (other-points problem)))

; ---------------------------------------------;
; -- GENERATE PROBLEMS ------------------------;
; ---------------------------------------------;

(let ( (current-point-id 0) )
  (defun reset-point-ids ()
    (setf current-point-id 0))

  (defun get-point-id ()
    (incf current-point-id)
    current-point-id)
)

(defun generate-problem (problem-name problem-dimension)
  (reset-point-ids)
  (let ( (start-end-point (generate-point)) )
    (make-instance 'problem
      :name problem-name
      :dimension problem-dimension
      :start-point start-end-point
      :end-point start-end-point
      :other-points (let ( (other-points ()) )
                      (dotimes (ii (1- problem-dimension) other-points)
                        (push (generate-point) other-points))))))

(defun generate-point (&optional (min-x 0.0) (max-x 20.0) (min-y 0.0) (max-y 20.0))
  (make-instance 'point
    :id (get-point-id)
    :x-pos (ut:random-number max-x :lower min-x)
    :y-pos (ut:random-number max-y :lower min-y)))

; same as generate-problem, but of type :tspreg
(defun generate-problem-with-regions (problem-name problem-dimension)
  (reset-point-ids)
  (let ( (start-end-point (generate-point-with-region)) )
    (make-instance 'problem
      :name problem-name
      :variant :tspreg
      :dimension problem-dimension
      :start-point start-end-point
      :end-point start-end-point
      :other-points (let ( (other-points ()) )
                      (dotimes (ii (1- problem-dimension) other-points)
                        (push (generate-point-with-region) other-points))))))

(defun generate-point-with-region (&optional (min 0.0) (max 20.0) (no-of-regions 3))
  (make-instance 'point :id (get-point-id)
                        :x-pos (ut:random-number max :lower min)
                        :y-pos (ut:random-number max :lower min)
                        :region-id (ut:random-number no-of-regions)))

; generiert Probleme direkt mit Clustern
(defun generate-cluster-problem (problem-name problem-dimension no-of-clusters)
  (let ( (cluster-centers (generate-cluster-centers no-of-clusters 5.0)) )
    (reset-point-ids)
    (multiple-value-bind (points-per-cluster additional-points) (floor problem-dimension no-of-clusters)
      (let*( (problem-points (mapcan #'(lambda (center)
                                         (format t "center: ~a~%" center)
                                         (let ( (min-x (- (x-pos center) 1.0))
                                                (max-x (+ (x-pos center) 1.0))
                                                (min-y (- (y-pos center) 1.0))
                                                (max-y (+ (y-pos center) 1.0)) )
                                           (let ( point-list )
                                             (dotimes (ii
                                                       (if (zerop (region-id center))
                                                         (+ points-per-cluster additional-points)
                                                         points-per-cluster)
                                                       point-list)
                                               (let ( (gen-pt (generate-point min-x max-x min-y max-y)) )
                                                 (setf (region-id gen-pt) (region-id center))
                                                 (push gen-pt point-list))))))
                                      cluster-centers))
             (start-end-point (nth (ut:random-number problem-dimension) problem-points)) )
        (make-instance 'problem
          :name problem-name
          :variant :tspreg
          :dimension problem-dimension
          :start-point start-end-point
          :end-point start-end-point
          :other-points (remove start-end-point problem-points))))))

(defun generate-cluster-centers (no-of-clusters min-distance &optional other-centers)
  (if (zerop no-of-clusters)
    other-centers
    (let ( (proposed-point (generate-point)) )
      (cond ( (every #'(lambda (x) (not (null x)))
                    (mapcar #'(lambda (pt) (>= min-distance (point-distance pt proposed-point))) other-centers))
              ; die every-Konstruktion ist umständlicher als nötig, aber aufgrund eines SBCL Bugs geht es nur so
              (setf (region-id proposed-point) (1- no-of-clusters))
              (generate-cluster-centers
                (1- no-of-clusters)
                min-distance
                (cons proposed-point other-centers)) )
            ( T
              (generate-cluster-centers no-of-clusters min-distance other-centers) )))))

; ---------------------------------------------;
; -- READ TSPLIB FORMAT -----------------------;
; ---------------------------------------------;
(defun read-tsplib-file (file &optional (scale-factor 1))
  (with-open-file (str (merge-pathnames file *default-data-directory*) :direction :input :if-does-not-exist :error)
    (multiple-value-bind (data-str problem-name problem-variant problem-dimension) (read-tsplib-header str)
      (multiple-value-bind (tsp-points dimension-correction)
                           (funcall
                             (if (eq problem-variant :tsp) #'read-tsplib-data #'read-tsplib-regions-data)
                             str problem-dimension scale-factor)
        (make-instance 'problem
          :name problem-name
          :dimension (+ problem-dimension dimension-correction)
          :variant problem-variant
          :start-point (first tsp-points)
          :end-point (first tsp-points)
          :other-points (rest tsp-points))))))

(defun read-tsplib-header (str &key name variant dimension)
  (case (read str)
    ('NODE_COORD_SECTION (values str name variant dimension))
    ('name (progn
             (read-char str)
             (read-tsplib-header str :name (read str))))
    ('type (progn
             (read-char str)
;             (unless (eq (read str) 'TSP) (warn "This problem is not of type TSP"))
             (read-tsplib-header str :name name
                                     :variant (case (read str)
                                       (TSP :tsp)
                                       (TSPREG :tspreg)
                                       (otherwise (warn "This problem is not of type TSP"))))))
    ('dimension (progn
                  (read-char str)
                  (let ( (dim-read (read str)) )
                    (read-tsplib-header str :name name :variant variant :dimension dim-read))))
    (otherwise (progn  ; includes COMMENT and EDGE_WEIGHT_TYPE
                  (read-line str)
                  (read-tsplib-header str :name name :variant variant :dimension dimension)))))

(defun read-tsplib-data (str expected-points scale-factor &optional read-data)
  (let ( (point-no (read str)) )
    (cond ( (eq point-no 'EOF)
            (values (reverse read-data) expected-points) )
          ( T
            (let*( (x (* scale-factor (read str)))
                   (y (* scale-factor (read str))) )
            (read-tsplib-data str (1- expected-points) scale-factor
                              (cons (make-instance 'point :id point-no :x-pos x :y-pos y) read-data)) )))))

(defun read-tsplib-regions-data (str expected-points scale-factor &optional read-data)
  (let ( (point-no (read str)) )
    (cond ( (eq point-no 'EOF)
            (values (reverse read-data) expected-points) )
          ( T
            (let*( (x (* scale-factor (read str)))
                   (y (* scale-factor (read str)))
                   (reg (read str)) )
            (read-tsplib-regions-data str (1- expected-points) scale-factor
              (cons (make-instance 'point :id point-no :x-pos x :y-pos y :region-id reg) read-data)) )))))

; ---------------------------------------------;
; -- WRITE TSPLIB FORMAT ----------------------;
; ---------------------------------------------;
(defun write-tsplib-file (file problem &key (comment "created by Alex"))
  (with-open-file (str (merge-pathnames file *default-data-directory*)
                   :direction :output :if-exists :rename-and-delete)
    (format str "NAME : ~a~%" (name problem))
    (format str "COMMENT : ~a~%" comment)
    (format str "TYPE : ~a~%" (variant problem))
    (format str "DIMENSION : ~a~%" (dimension problem))
    (format str "EDGE_WEIGHT_TYPE : EUC_2D~%")
    (format str "NODE_COORD_SECTION~%")
    (format str "1 ~a ~a~a~%" (x-pos (start-point problem))
                              (y-pos (start-point problem))
                              (if (eq (variant problem) :tsp)
                                ""
                                (format nil " ~a" (region-id (start-point problem)))))

    (let ( (counter 2) )
      (dolist (problem-point (other-points problem))
        (case (variant problem)
          ( :tsp
            (format str "~a ~a ~a~%" counter (x-pos problem-point) (y-pos problem-point)) )
          ( otherwise
            (format str "~a ~a ~a ~a~%" counter (x-pos problem-point) (y-pos problem-point)
                                        (region-id problem-point)) ))
        (incf counter)))
    (format str "EOF")
))

; ---------------------------------------------;
; -- PRINT TSPs -------------------------------;
; ---------------------------------------------;
(let ( (image-width 10)
       (image-height 10) )

  (defun print-tsp (problem &optional (output-directory *default-output-directory*))
    (set-problem-bounding-box problem)
    (destructuring-bind (problem-min-x problem-max-x problem-min-y problem-max-y)
                        (get-problem-knowledge problem :bounding-box)
;      (format t "problem size: ~a ~a ~a ~a~%" problem-max-x problem-min-x problem-max-y problem-min-y)
      (let*( (output-file (merge-pathnames output-directory
                                           (make-pathname :name (ut:->string (name problem))
                                                          :type "tex")))
             (scale-factor-x (/ image-width (- problem-max-x problem-min-x)))
             (scale-factor-y (/ image-height (- problem-max-y problem-min-y))) )
        (format t "output-file: ~a~%" output-file)
        (with-open-file (str output-file :direction :output :if-exists :supersede :if-does-not-exist :create)
          (format str "\\documentclass\{article\}~%")
          (format str "\\usepackage\{tikz\}~%")
          (format str "\\pagestyle\{empty\}~2%")
          (format str "\\begin\{document\}~%")
          (format str "\\begin\{tikzpicture\}~%");[y=-1cm]~%") Kommt aufs Problem an, ob das sinnvoll ist, bei Wiener Problemen jedenfalls nicht
          (funcall
            (if (eq (variant problem) :tsp)
              #'make-tikz-code-for-points
              #'make-tikz-code-for-region-points)
            str (start-point problem) (other-points problem)
            scale-factor-x scale-factor-y problem-min-x problem-min-y)
          (when (solution problem)
            (format str "\\draw ~{(~a)~^ -- ~};~%"
              (mapcar #'id (solution problem))))
          (format str "\\end\{tikzpicture\}~%")
          (format str "\\par\\vspace*{2ex}\\noindent found path: ~{~a~^, ~}~%"
            (mapcar #'id (solution problem)))
          (format str "\\par\\noindent path length: ~a~%" (evaluate-problem-solution problem))
          (format str "\\end\{document\}~%")))))

)

(defun make-tikz-code-for-points (str start-point other-points scale-factor-x scale-factor-y min-x min-y)
  (let ( point-numbers )
    (format str "\\path~%")
    (dolist (pt (cons start-point other-points))
      (push (id pt) point-numbers)
      (format str "(~,2f, ~,2f) coordinate (~a)~%"
        (* (- (x-pos pt) min-x) scale-factor-x)
        (* (- (y-pos pt) min-y) scale-factor-y)
        (id pt)))
    (format str ";~%")
    (format str "\\path plot[mark=*] coordinates \{~{(~a)~^ ~}\};~%" point-numbers)
    (format str "\\path[color=red] plot[mark=*] coordinates \{(~a)\};~%" (id start-point))
    (format str "\\foreach \\no in \{~{~a~^,~}\} \\node[anchor=east] at (\\no) \{\\no\};~%" point-numbers)))

(let ( (colors '("green!80!blue" "yellow!80!blue" "blue" "cyan" "brown")) )

  (defun get-color-for-reg (reg-no)
    (nth reg-no colors))

)


(defun make-tikz-code-for-region-points (str start-point other-points scale-factor-x scale-factor-y min-x min-y)
  (let ( point-numbers )
    (format str "\\path~%")
    (dolist (pt (cons start-point other-points))
      (push (cons (id pt) (region-id pt)) point-numbers)
      (format str "(~,2f, ~,2f) coordinate (~a)~%"
        (* (- (x-pos pt) min-x) scale-factor-x)
        (* (- (y-pos pt) min-y) scale-factor-y)
        (id pt)))
    (format str ";~%")
    (dolist (pt point-numbers)
      (format str "\\path[color=~a] plot[mark=*] coordinates \{(~a)\};~%"
        (get-color-for-reg (cdr pt))
        (car pt)))
    (format str "\\draw (~a) circle (4pt);" (id start-point))
;    (format str "\\path[color=red] plot[mark=*] coordinates \{(~a)\};~%" (id start-point))
    (format str "\\foreach \\no in \{~{~a~^,~}\} \\node[anchor=east] at (\\no) \{\\no\};~%"
                (mapcar #'car point-numbers))))

