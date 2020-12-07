#!/usr/bin/sbcl --script

(require "asdf")
(require "uiop")

(defvar *bags* (make-hash-table :test 'equal))

(defstruct rule 
  (n 0 :type integer)
  (color "" :type string))

(defun rules
  (words)
  (if (equal words '("no" "other" "bags."))
    (return-from rules nil))
  (if (not words)
    (return-from rules nil))
  (cons (make-rule 
          :n (parse-integer (car words)) 
          :color (format nil "~a ~a" (second words) (third words)))
        (rules (cddddr words))))


(defun has-bag
  (needle haystack)
  (loop for rule in (gethash haystack *bags*)
        do (if
             (equal (rule-color rule) needle)
             (return-from has-bag t)
             (if (has-bag needle (rule-color rule))
               (return-from has-bag t)))))

(defun bags-in
  (bag)
  (loop for rule in (gethash bag *bags*)
        sum (+
              (rule-n rule)
              (* (rule-n rule) (bags-in (rule-color rule))))))

(defvar *lines* 
  (with-open-file (f (or (second sb-ext:*posix-argv*) "../day7.input"))
    (loop for line = (read-line f nil)
          while line
          collect line)))

(loop for line in *lines*
      for words = (uiop:split-string line)
      for color = (format nil "~a ~a" (first words) (second words))
      for rules = (rules (cddddr words))
      do (setf (gethash color *bags*) rules))

(format t "part1: ~a~%"
        (loop for k being the hash-key of *bags*
              sum (if (has-bag "shiny gold" k) 1 0)))

(format t "part2: ~a~%" (bags-in "shiny gold"))
