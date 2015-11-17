;;;; Nonograms solver
;;;; Author: Aaron L. Zeng <a2z@mit.edu>
;;;; Last modified: Tuesday, November 17, 2015

;;; FIXME add pruning to solve-puzzle
;;; FIXME set package

(defclass puzzle ()
  ((height :reader height :initarg :height :documentation "number of rows"
           :initform (error "must specify height"))
   (width :reader width :initarg :width :documentation "number of columns"
          :initform (error "must specify width"))
   (cells :accessor cells :documentation "array of cells"
          :type '(array (or (eql :unknown) (eql :black) (eql :white)) 2))
   (horiz :accessor horiz :documentation "horizonal constraints")
   (verti :accessor verti :documentation "vertical constraints")))

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

(defun possible-rows (size constraint)
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
             if (solve-aux (1+ row))
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