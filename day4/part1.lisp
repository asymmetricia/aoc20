#!/usr/bin/sbcl --script

(require "asdf")
(require "split-sequence")

(defvar passports '())
(defvar passport '())

(defvar lines
  (with-open-file
    (f "part1/input")
    (loop for line = (read-line f nil)
          while line
          collect line)))

(loop for line in lines do
      (loop for pair in (split-sequence:split-sequence #\  line) do
            (cond
              ((string= pair "") (block nil
                     (setf passports (cons passport passports))
                     (setf passport '())))
              ((string= (subseq pair 0 4) "cid:") ())
              (t (setf passport (cons pair passport))))))

(if passport
  (setf passports (cons passport passports)))

(format t "~a ~%"
(loop for passport in passports
      sum (if (= (length passport) 7) 1 0)))
