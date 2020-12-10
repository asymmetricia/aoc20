#!/usr/bin/sbcl --script

; read a bunch of numbers. `read` read "representations" and not strings, so a number becomes a number
(defvar *nums*
  (sort
    (with-open-file (f "../day10.input")
      (loop for line = (read f nil)
            while line
            collect line))
    #'<))

; we start at 0 and end at (end+3)
(setf *nums* (cons 0 (append *nums* (list (+ (car (last *nums*)) 3)))))

; for 1 … end, compute the diff
(defvar *diffs*
  (loop for num in *nums*
        for y from 0
        if (> y 0)
        collect (- num (elt *nums* (1- y)))))

; count diffs
(defvar *ones* (loop for diff in *diffs*
                     if (= diff 1)
                     sum 1))

(defvar *threes* (loop for diff in *diffs*
                     if (= diff 3)
                     sum 1))

; solve part 1
(format t "ones: ~a; threes: ~a~%" *ones* *threes*)
(format t "part 1: ~a~%" (* *ones* *threes*))

; basic recursive traversal; but memoizing results gives us many orders of magnitude speed-up
(defvar *memo* (make-hash-table :test #'equal))
(defun path
  (prev bag)
  (unless bag (return-from path 1))
  (or
    (gethash (cons prev bag) *memo*)
    (setf (gethash (cons prev bag) *memo*)
          (+
            (path (first bag) (rest bag))
            (or
              (and (rest bag)
                 (<= (- (elt bag 1) prev) 3)
                 (path prev (rest bag)))
              0)))))

/(format t "~a~%" (path 0 (rest *nums*)))
