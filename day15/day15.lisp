#!/usr/bin/sbcl --script

(load "../common.lisp")

(defun speak (heard numbers turn stop &optional debug)
  (if debug (format t "turn ~a: speak ~a~%" (1- turn) heard))
  (if (= turn stop) (return-from speak heard))
  (let ((last (gethash heard numbers)))
    (setf (gethash heard numbers) turn)
    (if last
      (speak (- turn last) numbers (1+ turn) stop debug)
      (speak 0 numbers (1+ turn) stop debug))))

(defvar *mem* (make-hash-table :size 300000000))
(loop for num in (list 0 3) 
      for i from 1
      do (setf (gethash num *mem*) i))
(format t "demo1 turn 10: ~a~%" (speak 6 *mem* 3 10))

(clrhash *mem*)
(loop for num in (list 0 3) 
      for i from 1
      do (setf (gethash num *mem*) i))
(format t "demo1 turn 2020: ~a~%" (speak 6 *mem* 3 2020))

(defvar *lines* (input "day15"))
(defvar *numbers* (uiop:split-string (first *lines*) :separator ","))

(clrhash *mem*)
(loop for num in (subseq *numbers* 0 (1- (length *numbers*)))
    for turn from 1
    do (setf (gethash (parse-integer num) *mem*) turn))

(defvar *last* (parse-integer (car (last *numbers*))))
(format t "part1: ~a~%" (speak *last* *mem* (length *numbers*) 2020))
(format t "table: ~a~%" (hash-table-count *mem*))

(clrhash *mem*)
(loop for num in (subseq *numbers* 0 (1- (length *numbers*)))
    for turn from 1
    do (setf (gethash (parse-integer num) *mem*) turn))

(format t "part2: ~a~%" (speak *last* *mem* (length *numbers*) 30000000))
(format t "table: ~a~%" (hash-table-count *mem*))
