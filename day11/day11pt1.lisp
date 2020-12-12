#!/usr/bin/sbcl --script

(load "../common.lisp")

(defvar *debugx* -1)
(defvar *debugy* -1)

(defvar *lines* (input "day11"))

;(defvar lines (input "day11demo"))

(defun occupied
  (lines x y)
  (and
    (>= y 0)
    (<  y (length lines))
    (>= x 0)
    (<  x (length (first lines)))
    (eql #\# (elt (elt lines y) x))))

(defun neighbors
  (lines x y)
  (loop for nx in (list (1- x) x (1+ x)) sum
        (loop for ny in (list (1- y) y (1+ y)) sum
              (or
                (and
                  (not (and (= nx x) (= ny y)))
                  (occupied lines nx ny)
                  1)
                0)
              do (if (and (= x *debugx*) (= y *debugy*)) (format t "~d ~d -> ~d  ~%" nx ny (occupied lines nx ny))))))

(defun updates
  (lines)
  (loop for line in lines
        for y from 0
        append
        (remove 'nil
                (loop for c across line
                      for x from 0
                      for neighbors = (neighbors lines x y)
                      do (if (and (= x *debugx*) (= y *debugy*)) (format t "~d neigh~%" neighbors))
                      collect
                      (let ((x x) (y y))
                        (cond
                          ((and (eql c #\L) (= neighbors 0)) (lambda (lines) (setf (elt (elt lines y) x) #\#)))
                          ((and (eql c #\#) (>= neighbors 4)) (lambda (lines) (setf (elt (elt lines y) x) #\L)))))))))

(defun update
  (lines)
  (let (
        (updates (updates lines))
        (nextlines (loop for line in lines collect (copy-seq line))))
    (loop for update in updates do (funcall update nextlines))
    (values nextlines (length updates))))

(defun run
  (lines)
  (moveto 1 1)
  (multiple-value-bind
    (nextlines updates) 
    (update lines)
    (printlines lines)
    (if (> updates 0) (run nextlines) nextlines)))

(defun printlines (lines) (moveto 1 1)
  (loop for line in lines do (format t "~a~%" line)))

(clear)
(defvar pt1 (run *lines*))
(format t "part1: ~d~%" (loop for line in pt1 sum (loop for c across line sum (if (eql c #\#) 1 0))))
