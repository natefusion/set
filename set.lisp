(defpackage set
  (:use :cl))

(in-package :set)

(defun ! (n) (declare (fixnum n))
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
  (the integer (/ (! n) (* (! r) (! (- n r))))))

(defun permutations (seq &key (r (length seq)) (repetitions nil)) (declare (simple-string seq) (fixnum r) (boolean repetitions))
  (let ((offset (if repetitions 0 1))
        (final (make-array (if repetitions
                               (expt (length seq) r)
                               (nPr (length seq) r))
                           :fill-pointer 0)))
    (declare ((vector string) final))
    (labels ((rotate (c) (declare (simple-string c))
               (dotimes (i (1- (length c)))
                 (rotatef (schar c i) (schar c (1+ i)))))
             (recur (r b c) (declare (fixnum r) (simple-string b c))
               (case r
                 (1 (loop for char across c
                          do (vector-push (string char) final)))
                 (2 (loop repeat (length c)
                          for c1 = (string (schar c 0))
                          do (loop for j from offset below (length c)
                                   for c2 = (string (schar c j))
                                   do (vector-push (concatenate 'string b c1 c2) final))
                             (rotate c)))
                 (t (loop repeat (length c)
                          do (recur (1- r) (concatenate 'string b (subseq c 0 1)) (subseq c 1))
                             (rotate c))))))
      (recur r "" seq)
      final)))

(defun combinations (seq r &key (repetitions nil)) (declare (simple-string seq) (fixnum r) (boolean repetitions))
  (let* ((offset (if repetitions 0 1))
         (n (length seq))
         (final (make-array (if repetitions
                                (/ (! (1- (+ n r)))
                                   (* (! r) (! (1- n))))
                                (nCr n r))
                            :fill-pointer 0)))
    (declare ((vector string) final))
    (labels ((recur (i r c) (declare (fixnum i r) (simple-string c))
               (loop for char across (subseq seq i)
                     for j fixnum from i
                     for combination = (concatenate 'string c (string char))
                     do (if (= r 1)
                            (vector-push combination final) 
                            (recur (+ j offset) (1- r) combination)))))
      (recur 0 r "")
      final)))

(defun contains (seq pat)
  (declare (string seq pat))
  (loop with pat-perms = (permutations pat)
        for x from 0 to (- (length seq) (length pat))
        for wat = (subseq seq x (+ x (length pat)))
        do (loop for y across pat-perms
                 do (when (string= y wat)
                      (return-from contains t)))))
