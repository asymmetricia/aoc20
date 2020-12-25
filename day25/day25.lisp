#!/usr/bin/sbcl --script

(load "../common.lisp")

(defvar *lines* (input "day25"))
(defvar *doorKey* (parse-integer (first *lines*)))
(defvar *cardKey* (parse-integer (second *lines*)))

(defun transform (subject loops &optional result)
  (if (= loops 0) result
    (transform subject (1- loops) (mod (* (or result 1) subject) 20201227))))

(assert (= (transform 7 8) 5764801))

(defun untransform (subject result &optional loops)
  (let ((loops (or loops 0)))
    (if (= result 1) loops
      (if (= (mod result subject) 0)
        (untransform subject (/ result subject) (1+ loops))
        (untransform subject (+ result 20201227) loops)))))

(assert (= (untransform 7 5764801) 8))

(format t "~a~%" (transform *cardKey* (untransform 7 *doorKey*)))
