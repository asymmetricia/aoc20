#!/usr/bin/sbcl --script

(load "../common.lisp")

(defvar *demo* (list
                 "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
                 "mem[8] = 11"
                 "mem[7] = 101"
                 "mem[8] = 0"))

(defvar lines (input "day14"))

(defun parsemask
  (line)
  (loop for c across (subseq line (+ (position #\= line) 2))
        for y downfrom (- (length line) 8)
        if (or (eql c #\0) (eql c #\1))
        collect (cons y c)))

(defun applymask
  (mask value)
  (if (not mask) (return-from applymask value))
  (let ((pos (car (first mask)))
        (val (cdr (first mask))))
    (case val
      (#\0 (applymask (rest mask) (logandc2 value (ash 1 pos))))
      (#\1 (applymask (rest mask) (logior value (ash 1 pos)))))))

(defun setmem (line memory mask)
  (let ((addr (parse-integer (subseq line (1+ (position #\[ line)) (position #\] line))))
        (value (applymask mask (parse-integer (subseq line (+ (position #\= line) 2))))))
    (if (assoc addr memory)
      (substitute (cons addr value) (assoc addr memory) memory)
      (append memory (cons (cons addr value) nil)))))

(defun run
  (lines memory mask)
  (unless lines (return-from run memory))
  (let ((line (first lines))
        (op (subseq (first lines) 0 3))
        (lines (rest lines)))
    (cond
      ((equal op "mas") (run lines memory (parsemask line)))
      ((equal op "mem") (run lines (setmem line memory mask) mask)))))

(format t "~a~%" (run *demo* 'nil 'nil))

(defvar memory (run lines 'nil 'nil))
(format t "~a~%"
        (loop for cell in memory sum (cdr cell)))
