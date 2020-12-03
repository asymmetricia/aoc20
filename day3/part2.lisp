#!/usr/bin/sbcl --script

(defvar lines
  (with-open-file 
    (f "input")
    (loop for line = (read-line f nil)
          while line
          collect line)))

(defun tree
  (lines x y dx dy)
  (if
    (not lines)
    (return-from tree 0))
  (if
    (> y 0)
    (return-from tree (tree (rest lines) x (1- y) dx dy)))
  (+
    (if (eq #\# (char (first lines) x) ) 1 0)
    (tree (rest lines) (mod (+ x dx) (length (first lines))) (1- dy) dx dy)))

(format t "~a ~%"
  (reduce
    #'*
    (loop for slope in '( (1 1) (3 1) (5 1) (7 1) (1 2))
          collect (tree lines 0 0 (elt slope 0) (elt slope 1)))))
