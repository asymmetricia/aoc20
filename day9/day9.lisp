#!/usr/bin/sbcl --script

(defvar *nums*
  (with-open-file (f "../day9.input")
    (loop for line = (read-line f nil)
          while line
          collect (parse-integer line))))

(defun validate
  (idx)
  (loop for i from (- idx 25) to (1- idx) do
        (loop for j from (- idx 25) to (1- idx)
              unless (eql i j)
              do (if (eql (elt *nums* idx) (+ (elt *nums* i) (elt *nums* j))) (return-from validate t))))
  (return-from validate 'nil))

(defun find-invalid ()
  (loop for i in *nums*
        for y from 0
        if (>= y 25)
        unless (validate y)
        do (return-from find-invalid i)))

(defvar *invalid* (find-invalid))
(format t "first invalid: ~a~%" *invalid*)

(defun find-seq (invalid)
  (loop for i from 0 to (- (length *nums*) 2) do
        (loop for j from (+ i 2) to (length *nums*)
              for sum = (reduce #'+ (subseq *nums* i j))
              until (> sum invalid)
              if (= sum invalid)
              do (return-from find-seq (values i j)))))

(multiple-value-bind (begin end) (find-seq *invalid*)
  (let
    ((sorted (sort (subseq *nums* begin end) #'<)))
    (format t "weakness: ~a~%" (+ (first sorted) (car (last sorted))))))
