#!/usr/bin/sbcl --script

(load "../common.lisp")

(defvar *lines* (input "day18"))

; lex parses a line to an expression
(defun lex (line)
  (loop for c across line
        if (not (equal c #\ ))
        collect
        (case c
          (#\+ 'plus)
          (#\* 'times)
          (#\( 'open)
          (#\) 'close)
          (t (parse-integer (string c))))))

(assert (equal (lex "1 + 1") (list 1 'plus 1)))

; PART 1

(defun eval1 (expr)
  (if (= (length expr) 1) (return-from eval1 (first expr)))
  (let* ((open (position 'open expr :from-end t))
         (close (and open (position 'close expr :start open))))
    (if open
      (return-from 
        eval1
        (eval1
          (append 
            (subseq expr 0 open) 
            (list (eval1 (subseq expr (1+ open) close)) )
            (subseq expr (1+ close)))))))
  (let* ((plus  (or (position 'plus  expr) (length expr)))
         (times (or (position 'times expr) (length expr)))
         (idxA (1- (min plus times)))
         (idxB (1+ (min plus times)))
         (opA (elt expr idxA))
         (opB (elt expr idxB))
         (result (if (< plus times) (+ opA opB) (* opA opB))))
    (return-from 
      eval1
      (eval1
        (append
          (subseq expr 0 idxA)
          (list result)
          (subseq expr (1+ idxB)))))))

(assert (equal (eval1 (lex "1 + (2 + 1)")) 4))
(assert (equal (eval1 (lex "1 + (2 * 1)")) 3))
(assert (equal (eval1 (lex "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")) 13632))

(format t "Part 1: ~a~%" (loop for line in *lines* sum (eval1 (lex line))))


; PART 2

(defun eval2 (expr)
  (if (= (length expr) 1) (return-from eval2 (first expr)))
  (let* ((open (position 'open expr :from-end t))
         (close (and open (position 'close expr :start open))))
    (if open
      (return-from 
        eval2
        (eval2
          (append 
            (subseq expr 0 open) 
            (list (eval2 (subseq expr (1+ open) close)) )
            (subseq expr (1+ close)))))))
  (let* ((plus (position 'plus expr)))
    (if plus
      (return-from
        eval2
        (eval2
          (append
            (subseq expr 0 (1- plus))
            (list (+ (elt expr (1- plus)) (elt expr (1+ plus))))
            (subseq expr (+ 2 plus)))))))
  (let* ((times (position 'times expr)))
    (if times
      (return-from
        eval2
        (eval2
          (append
            (subseq expr 0 (1- times))
            (list (* (elt expr (1- times)) (elt expr (1+ times))))
            (subseq expr (+ 2 times))))))))

(assert (equal (eval2 (lex "1 + (2 + 1)")) 4))
(assert (equal (eval2 (lex "1 + (2 * 1)")) 3))
(assert (equal (eval2 (lex "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")) 23340))

(format t "Part 2: ~a~%" (loop for line in *lines* sum (eval2 (lex line))))
