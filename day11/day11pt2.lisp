#!/usr/bin/sbcl --script

(load "../common.lisp")

(defvar *debugx* -1)
(defvar *debugy* -1)

(defvar *lines* (input "day11"))
;(defvar *lines* (input "day11demo"))

(defun neighbor (lines x y dx dy)
  "neighbor returns the non-empty neighbor in the direction (dx,dy) from (x,y), either # or L. Something else is returned if nothing is found in-bounds"
  (let
    ((cx (+ x dx))
     (cy (+ y dy)))
    (values
      (cond
        ((< cx 0) #\.)
        ((>= cx (length (first lines))) #\.)
        ((< cy 0) #\.)
        ((>= cy (length lines)) #\.)
        ((eql (elt (elt lines cy) cx) #\.) (neighbor lines cx cy dx dy))
        (t (elt (elt lines cy) cx)))
      cx cy)))

    
(defun neighbors
  (lines x y)
  (loop for dx in (list -1 0 1) append
        (loop for dy in (list -1 0 1) append
              (unless 
                (= dx dy 0) 
                (list (neighbor lines x y dx dy))))))

(defun updates
  (lines)
  (loop for line in lines
        for y from 0
        append
        (remove 'nil
                (loop for c across line
                      for x from 0
                      for neighbors = (count-if (lambda (c) (eql c #\#)) (neighbors lines x y))
                      do (if (and (= x *debugx*) (= y *debugy*)) (format t "~d neigh~%" neighbors))
                      collect
                      (let ((x x) (y y))
                        (cond
                          ((and (eql c #\L) (= neighbors 0)) (lambda (lines) (setf (elt (elt lines y) x) #\#)))
                          ((and (eql c #\#) (>= neighbors 5)) (lambda (lines) (setf (elt (elt lines y) x) #\L)))))))))

(defun update
  (lines)
  (let (
        (updates (updates lines))
        (nextlines (loop for line in lines collect (copy-seq line))))
    (loop for update in updates do (funcall update nextlines))
    (values nextlines (length updates))))

(defun printlines (lines) (moveto 1 1)
  (loop for line in lines do (format t "~a~%" line)))

(defun run (lines)
  (multiple-value-bind
    (lines updates) (update lines)
    (moveto 1 1)
    (printlines lines)
    (if (> updates 0) (run lines) lines)))

(clear)
(format t "~d~%"
        (loop for line in (run *lines*) sum
              (loop for c across line sum
                    (if (eql c #\#) 1 0))))
              
