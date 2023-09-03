(defpackage set
  (:use :cl))

(in-package :set)

(defun factorial (n) (declare (fixnum n))
  (loop with result of-type integer = 1
        for x from 2 to n
        do (setf result (* x result))
        finally (return result)))

(defun nPr (n r) (declare (fixnum n r))
  (do ((result 1)
       (x n (1- x))
       (i 0 (1+ i)))
      ((= i r) result)
    (declare (integer x i result))
    (setf result (* x result))))

(defun nCr (n r) (declare (fixnum n r))
  (the integer (/ (nPr n r) (factorial (- n r)))))

(defun contains (seq pat)
  (declare (string seq pat))
  (loop with pat-perms = (permutations pat)
        for x from 0 to (- (length seq) (length pat))
        for wat = (subseq seq x (+ x (length pat)))
        do (loop for y across pat-perms
             do (when (string= y wat)
                  (return-from contains t)))))

(defun permutations (seq) (declare (simple-string seq) (optimize speed))
  (setf seq (copy-seq seq)) ; pass by value, not by reference (not literally, but effectively)
  (let* ((len (length seq))
         (final (make-array (factorial len) :initial-element "" :fill-pointer 0)))
    (labels ((nswap (a b &aux (temp (aref seq a)))
               (setf (aref seq a) (aref seq b)
                     (aref seq b) temp))
             (internal (s) (declare (fixnum s))
               (if (= s len)
                   (vector-push (copy-seq seq) final)
                   (loop for i from s below len
                         do (nswap s i)
                            (internal (1+ s))
                            (nswap s i)))))
      (internal 0)
      final)))

(defun combinations (seq r) (declare (simple-string seq) (fixnum r) (optimize speed))
  (let ((final (make-array (choose (length seq) r) :fill-pointer 0)))
    (labels ((recur (i r c) (declare (fixnum i r) (simple-string c))
               (loop for char across (subseq seq i)
                     for j fixnum from i
                     for combination = (concatenate 'string c (string char))
                     do (if (= r 1)
                            (vector-push combination final) 
                            (recur (1+ j) (1- r) combination)))))
      (recur 0 r "")
      final)))
