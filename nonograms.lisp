;;;; Nonograms solver
;;;; Author: Aaron L. Zeng <a2z@mit.edu>
;;;; Last modified: Tuesday, November 17, 2015

;;; FIXME add pruning to solve-puzzle
;;; FIXME set package
;;; TODO add constraints to puzzle printout

(defclass puzzle ()
  ((height :reader height :initarg :height :documentation "number of rows"
           :initform (error "must specify height")
           :type 'fixnum)
   (width :reader width :initarg :width :documentation "number of columns"
          :initform (error "must specify width")
          :type 'fixnum)
   (cells :accessor cells :documentation "array of cells"
          :type '(simple-array (or (eql :unknown) (eql :black) (eql :white)) 2))
   (horiz :accessor horiz :documentation "horizonal constraints"
          :type '(simple-array list))
   (verti :accessor verti :documentation "vertical constraints"
          :type '(simple-array list))))

(defmethod initialize-instance :after ((puzzle puzzle) &key)
  (setf (cells puzzle)
        (make-array (list (height puzzle) (width puzzle))
                    :element-type '(or (eql :unknown) (eql :black) (eql :white))
                    :initial-element :unknown))
  (setf (horiz puzzle)
        (make-array (width puzzle)))
  (setf (verti puzzle)
        (make-array (height puzzle))))

(defun read-puzzle (height width &optional (stream *standard-input*))
  (let ((puzzle (make-instance 'puzzle :height height :width width)))
    (dotimes (i height)
      (setf (svref (horiz puzzle) i)
            (read-delimited-list #\; stream)))
    (dotimes (j width)
      (setf (svref (verti puzzle) j)
            (read-delimited-list #\; stream)))
    puzzle))

(defun get-row (puzzle i)
  (loop for j below (width puzzle)
     collect (aref (cells puzzle) i j)))

(defun get-col (puzzle j)
  (loop for i below (height puzzle)
     collect (aref (cells puzzle) i j)))

(defun get-runs (line)
  ;; list of (run-size . capped)
  (let (runs (run-size 0) left-cap)
    (dolist (cell line)
      (ecase cell
        (:white
         (when (plusp run-size)
           (push (cons run-size left-cap) runs)
           (setf run-size 0))
         (setf left-cap t))
        (:unknown
         (when (plusp run-size)
           (push (cons run-size nil) runs)
           (setf run-size 0)))
        (:black
         (incf run-size))))
    (when (plusp run-size)
      (push (cons run-size left-cap) runs))
    (nreverse runs)))

(defmacro defmemo (func-name lambda-list &body body)
  (let ((hash-name (gensym)))
    `(let ((,hash-name (make-hash-table :test #'equal)))
       (defun ,func-name ,lambda-list
         (multiple-value-bind (result found)
             (gethash (list ,@lambda-list) ,hash-name)
           (if found
               result
               (setf (gethash (list ,@lambda-list) ,hash-name)
                     (progn ,@body))))))))

(defmemo possible-rows (size constraint)
  (defun possibilities-padded (size constraint) ; call with size + 1, one padding mandatory
    (cond
      ((minusp size) nil)
      ((null constraint) (list (loop repeat size collect :white)))
      ((zerop size) nil)
      (t
       (loop for pad from 1 upto size
          for suffixes = (possibilities-padded (- size (first constraint) pad) (rest constraint))
          for prefix = (nconc (loop repeat pad collect :white)
                              (loop repeat (first constraint) collect :black))
          nconc (mapcar #'(lambda (suffix) (append prefix suffix))
                        suffixes)))))
  (mapcar #'cdr (possibilities-padded (1+ size) constraint))) ; remove initial padding

(defmemo get-complete-runs (line end)
  (let (runs (run-size 0))
    (dolist (cell (subseq line 0 end))
      (ecase cell
        (:black
         (incf run-size))
        (:white
         (when (plusp run-size)
           (push run-size runs)
           (setf run-size 0)))
        (:unknown
         (error "Can only call get-complete-runs on known lines"))))
    (when (plusp run-size)
      (if (< end (length line))
          (when (eql :white (nth end line))
            (push run-size runs))
          (push run-size runs)))
    (nreverse runs)))

(defmemo constraint-match-prefix-p (line constraint end)
  (let ((runs (get-complete-runs line end)))
    (and (>= (length constraint) (length runs))
         (equal (subseq constraint 0 (length runs))
                runs))))

(defun constraint-match-p (line constraint)
  (equal
   constraint
   (let (runs (run-size 0))
     (dolist (cell line)
       (ecase cell
         (:black
          (incf run-size))
         (:white
          (when (plusp run-size)
            (push run-size runs)
            (setf run-size 0)))
         (:unknown
          (error "Can only call constraint-match-p on known lines"))))
     (when (plusp run-size)
       (push run-size runs))
     (nreverse runs))))

(defun puzzle-prefix-ok-p (puzzle rows)
  (loop
     for j below (width puzzle)
     for line = (get-col puzzle j)
     if (not (constraint-match-prefix-p line (svref (verti puzzle) j) rows))
     return nil
     finally (return t)))

(defun puzzle-ok-p (puzzle)
  (loop
     for j below (width puzzle)
     for line = (get-col puzzle j)
     if (not (constraint-match-p line (svref (verti puzzle) j)))
     return nil
     finally (return t)))

(defun solve-puzzle (puzzle)
  ;; FIXME pruning for large puzzles
  (defun solve-aux (row)
    (if (>= row (height puzzle))
        (and (puzzle-ok-p puzzle)
             puzzle)
        (let ((original-row (get-row puzzle row)))
          (loop
             for candidate in (possible-rows (width puzzle)
                                             (svref (horiz puzzle) row))
             do (loop
                   for j below (width puzzle)
                   for cell in candidate
                   do (setf (aref (cells puzzle) row j) cell))
             if (and (puzzle-prefix-ok-p puzzle (1+ row))
                     (solve-aux (1+ row)))
             do (return-from solve-aux puzzle))
          (loop for j below (width puzzle)
             for cell in original-row
             do (setf (aref (cells puzzle) row j) cell)))))

  (dotimes (i (height puzzle))
    (when (loop for j below (width puzzle)
             thereis (eql :unknown (aref (cells puzzle) i j)))
      (return-from solve-puzzle
        (and (solve-aux i) puzzle))))
  (and (puzzle-ok-p puzzle) puzzle))

(defmethod print-object ((puzzle puzzle) stream)
  (fresh-line stream)
  (loop repeat (1- (* 2 (width puzzle)))
     do (princ #\- stream))
  (terpri stream)
  (format stream "~{~{~a~^ ~}~%~}"
          (loop for i below (height puzzle)
             collect (mapcar #'(lambda (sym)
                                 (ecase sym
                                   (:white " ")
                                   (:black "X")
                                   (:unknown ".")))
                             (get-row puzzle i))))
  (loop repeat (1- (* 2 (width puzzle)))
     do (princ #\- stream))
  (terpri stream))
