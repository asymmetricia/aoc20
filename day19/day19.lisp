#!/usr/bin/sbcl --script

(load "../common.lisp")

(defvar *lines* (input "day19"))

(defvar *rule-strings* (loop for line in *lines* until (equal line "") collect line))
(defvar *predicates* (subseq *lines* (1+ (length *rule-strings*))))

(defstruct rule type payload)

; rule-apply recursively applies the given rule, returning true if there's 
; ultimately a path through the rule graph that matches. It returns a list of
; the options for how many characters the rule can consume, or nil if it cannot
; consume any (i.e., if it does not match.)
(defun rule-apply (rules rule input)
  (ecase (rule-type rule)
    ('char (rule-apply-char rules (rule-payload rule) input))
    ('seq (rule-apply-seq rules (rule-payload rule) input))
    ('choice (rule-apply-choice rules (rule-payload rule) input))
    ('empty (rule-apply-empty input))))

(defun rule-apply-empty (input)
  (if (equal input "") '(0)))

(defun rule-apply-char (rules c input)
  (cond
    ((equal input "") 'nil)
    ((eql c (elt input 0)) '(1))))

(defun rule-apply-seq (rules seq input)
  (let ((result (rule-apply rules (gethash (first seq) rules) input)))
    (if (= (length seq) 1)
      result
      (loop for consumed in result 
            for nextresult = (rule-apply-seq rules (rest seq) (subseq input consumed))
            if nextresult
            append (loop for nextconsumed in nextresult collect (+ consumed nextconsumed))))))

(defun rule-apply-choice (rules seq input)
  (let
    ((resultA (rule-apply-seq rules (elt seq 0) input))
     (resultB (rule-apply-seq rules (elt seq 1) input)))
    (append resultA resultB)))


(defun parse-rules (rule-strings)
  (let ((rules (make-hash-table :test #'equal)))
    (setf (gethash "root" rules) (make-rule :type 'seq :payload '("0" "empty")))
    (setf (gethash "empty" rules) (make-rule :type 'empty))
    (loop for rule in rule-strings
          for colon = (position #\: rule)
          for num = (subseq rule 0 colon) do
          (setf (gethash num rules) (parse-rule rule)))
    rules))

(defun parse-rule (rule)
  (let* ((colon (position #\: rule))
         (quot (position #\" rule))
         (pipe (position #\| rule)))
    (cond
      (quot (make-rule 
              :type 'char 
              :payload (elt rule (1+ quot))))
      (pipe (make-rule 
              :type 'choice 
              :payload (list
                         (uiop:split-string (subseq rule (+ 2 colon) (1- pipe)) :separator " ")
                         (uiop:split-string (subseq rule (+ 2 pipe)) :separator " "))))
      (t (make-rule 
           :type 'seq 
           :payload (uiop:split-string (subseq rule (+ 2 colon)) :separator " "))))))

(defvar *rules* (parse-rules *rule-strings*))


(defvar *test-rules* (parse-rules '("0: \"a\"" "1: 0 0" "2: 0 | 0 2")))
(assert (equal (rule-apply *test-rules* (gethash "0" *test-rules*) "a") '(1)))
(assert (equal (rule-apply *test-rules* (gethash "1" *test-rules*) "aa") '(2)))
(assert (equal (rule-apply *test-rules* (gethash "1" *test-rules*) "a") 'nil))
(assert (equal (rule-apply *test-rules* (gethash "2" *test-rules*) "aaa") '(1 2 3)))

(defvar *demo-rules* (parse-rules (loop for line in (input "day19" ".demo") until (equal line "") collect line)))
(assert (rule-apply *demo-rules* (gethash "root" *demo-rules*) "bbabbbbaabaabba"))

(format t "Part 1: ~a~%" (loop for pred in *predicates* 
                               if (rule-apply *rules* (gethash "root" *rules*) pred)
                               sum 1))

(setf (gethash "8" *rules*) (parse-rule "8: 42 | 42 8"))
(setf (gethash "11" *rules*) (parse-rule "11: 42 31 | 42 11 31"))

(format t "Part 2: ~a~%" (loop for pred in *predicates* 
                               if (rule-apply *rules* (gethash "root" *rules*) pred)
                               sum 1))
