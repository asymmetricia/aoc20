#!/usr/bin/sbcl --script

(load "../common.lisp")

(defvar *lines* (input "day17"))

(defun parse (dimensions lines)
  (let* ((state (make-hash-table :test #'equal))
         (zeros (loop for _ from 0 to (- dimensions 3) collect 0)))
    (loop for line in lines
          for y from 0
          do (loop for c across line
                   for x from 0
                   if (eql c #\#)
                   do (setf (gethash (append (list x y) zeros) state) t)))
    state))

(defun bounds (dimensions input)
  (loop for dimension from 0 to (1- dimensions)
        collect (list
                  (loop for coord being the hash-key of input
                        minimize (elt coord dimension))
                  (loop for coord being the hash-key of input
                        maximize (elt coord dimension)))))

(defun neighbors (coord)
  (let ((ord (first coord)))
    (if (= (length coord) 1)
      (loop for i from (1- ord) to (1+ ord) collect (list i) )
      (loop for i from (1- ord) to (1+ ord) append
            (loop for neigh in (neighbors (rest coord))
                  collect (append (list i) neigh))))))

(assert (equal (neighbors '(0)) '((-1) (0) (1))))
(assert (equal (neighbors '(0 0)) '((-1 -1) (-1 0) (-1 1) (0 -1) (0 0) (0 1) (1 -1) (1 0) (1 1))))

(defun active-neighbors (coord input)
  (count-if (lambda (x) (and
                          (not (equal x coord))
                          (gethash x input))) 
            (neighbors coord)))

(assert (equal (active-neighbors '(0 0) (parse 2 '("#.." "..." "..."))) 0))
(assert (equal (active-neighbors '(1 0) (parse 2 '("#.." "..." "..."))) 1))


(defun cycle-cell (dimensions coord input output)
  (let ((active (active-neighbors coord input)))
    (if (or
          (= active 3)
          (and (= active 2) (gethash coord input)))
      (setf (gethash coord output) t))))

(defun cycle-dimension (dimensions bounds input output &optional coord)
  (let* ((bound (first bounds))
          (last (= (length bounds) 1)))
     (loop for d from (1- (first bound)) to (1+ (elt bound 1))
           do (let* ((coord (append coord (cons d nil))))
                (if last
                  (cycle-cell dimensions coord input output)
                  (cycle-dimension dimensions (rest bounds) input output coord))))))

(defun cycle (dimensions input)
  (let* ((output (make-hash-table :test #'equal)))
    (cycle-dimension dimensions (bounds dimensions input) input output)
    output))

(defun print-state (dimensions input &optional bounds coord)
  (let* ((bounds (or bounds (bounds dimensions input)))
        (bound (car (last bounds))))
    (if (= (length bounds) 1)
      (format t "~{~a~}~%"
        (loop for i from (elt bound 0) to (elt bound 1) 
              for _coord = (append (list i) coord)
              for cell = (gethash _coord input)
              collect (if cell #\# #\.)))
      (loop for i from (elt bound 0) to (elt bound 1)
            do (print-state 
                 dimensions 
                 input
                 (butlast bounds)
                 (append (list i) coord))
            (if (> (length bounds) 2) (format t "~%"))))))

(defun cycles (dimensions input n)
  (cycle dimensions (if (= n 1) 
                      input
                      (cycles dimensions input (1- n)))))


(defvar *demo* (parse 3 '(".#." "..#" "###")))
(assert (equal 112 (hash-table-count (cycles 3 *demo* 6))))

(format t "Part 1: ~a~%" (hash-table-count (cycles 3 (parse 3 *lines*) 6)))
(format t "Part 2: ~a~%" (hash-table-count (cycles 4 (parse 4 *lines*) 6)))
