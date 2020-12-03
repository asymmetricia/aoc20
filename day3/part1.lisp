#!/usr/bin/sbcl --script

(defvar lines
  (with-open-file 
    (f "input")
    (loop for line = (read-line f nil)
          while line
          collect line)))

(defun tree
  (lines x dx)
  (if
    (not lines)
    (return-from tree 0))
  (+
    (if (eq #\# (char (first lines) x) ) 1 0)
    (tree (rest lines) (mod (+ x dx) (length (first lines))) dx)))

(format t "~a ~%" (tree lines 0 3))
