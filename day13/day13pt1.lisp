#!/usr/bin/sbcl --script

(load "../common.lisp")

(defvar lines (input "day13"))
(defvar *arrival* (parse-integer (first lines)))
(defvar *busses* (loop for bus in (uiop:split-string (elt lines 1) :separator ",") if (not (equal bus "x")) collect (parse-integer bus)))

(defun seek
  (x busses)
  (let ((departures
          (loop for bus in busses
                for mod = (mod x bus)
                if (= mod 0)
                collect bus)))
    (if (first departures)
      (values x (first departures))
      (seek (1+ x) busses))))

(multiple-value-bind 
  (x bus) 
  (seek *arrival* *busses*)
  (format t "~a ~a => ~a~%" x bus (* (- x *arrival*) bus)))
