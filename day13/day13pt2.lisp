#!/usr/bin/sbcl --script

(load "../common.lisp")

(defvar lines (input "day13"))
(defvar *busses* (loop for bus in (uiop:split-string (elt lines 1) :separator ",") collect (or (parse-integer bus :junk-allowed t) -1)))

(defun seek (offset delta idx busses)
  (cond
    ((= idx (length busses))
     offset)
    ((= -1 (elt busses idx)) 
     (seek offset delta (1+ idx) busses))
    ((= 0 (mod (+ offset idx) (elt busses idx))) 
     (seek offset (* delta (elt busses idx)) (1+ idx) busses))
    (t
      (seek (+ offset delta) delta idx busses))))

(format t "~a~%" (seek 0 1 0 *busses*))
