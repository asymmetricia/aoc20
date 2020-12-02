#!/usr/bin/env -S sbcl --script

(defun ruleMin (rule)
  (let ((hyphen (position #\- rule)))
    (if hyphen (parse-integer (subseq rule 0 hyphen)))))

(defun ruleMax (rule)
  (let ((hyphen (position #\- rule))
        (space (position #\  rule)))
    (if hyphen (parse-integer (subseq rule (1+ hyphen) space)))))

(defun ruleChar (rule)
  (let ((space (position #\  rule)))
    (if space (char rule (1+ space)))))

(defun rulePass (rule)
  (let ((colon (position #\: rule)))
    (if colon (subseq rule (+ colon 2)))))
       
(defun ruleCheck (rule)
  (let ((rmin (ruleMin rule))
        (rmax (ruleMax rule))
        (rchar (ruleChar rule))
        (pass (rulePass rule)))
    (let ((occurrences (reduce #'+ (map 'list (lambda (x) (if (eq x rchar) 1 0)) pass))))
      (and (>= occurrences rmin) (<= occurrences rmax)))))

(defvar input (if (= (length sb-ext:*posix-argv*) 2) (elt sb-ext:*posix-argv* 1) "input"))

(defvar data
  (with-open-file (f input) 
    (loop for line = (read-line f nil)
          while line
          collect line)))

(loop for rule in data
      do (format t "~a ~a ~%" rule (ruleCheck rule)))

(format t "~a ~%" (loop for rule in data
      count (ruleCheck rule)))
