#!/usr/bin/sbcl --script

(defvar lines
  (with-open-file
    (f "part1/input")
    (loop for line = (read-line f nil)
          while line
          collect line)))

(defun lower
  (pair)
  (list (first pair) (floor (/ (+ (first pair) (elt pair 1)) 2))))

(defun upper
  (pair)
  (list (ceiling (/ (+ (first pair) (elt pair 1)) 2)) (elt pair 1)))

(defun seatId
  (pass)
  (let ((row (list 0 127)) (col '(0 7)))
    (loop for c across pass do
          (if (eq c #\F) (setf row (lower row)))
          (if (eq c #\B) (setf row (upper row)))
          (if (eq c #\L) (setf col (lower col)))
          (if (eq c #\R) (setf col (upper col))))
    (+ (* (first row) 8) (first col))))

(defvar seats
  (sort
    (loop for line in lines collect (seatId line))
    #'<))

(defun seek
  (prev seats)
  (if (not prev)
    (seek (first seats) (rest seats))
    (if
      (eq (1+ prev) (first seats))
      (seek (first seats) (rest seats))
      (1+ prev))))

(format t "~a ~%" (apply #'max seats))
(format t "~a ~%" (seek 'nil seats))
