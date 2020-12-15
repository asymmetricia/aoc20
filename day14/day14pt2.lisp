#!/usr/bin/sbcl --script

(load "../common.lisp")

(defvar *demo* (list
								 "mask = 000000000000000000000000000000X1001X"
								 "mem[42] = 100"
								 "mask = 00000000000000000000000000000000X0XX"
								 "mem[26] = 1"))

(defvar lines (input "day14"))

(defun parsemask
  (line)
  (loop for c across (subseq line (+ (position #\= line) 2))
        for y downfrom (- (length line) 8)
        if (or (eql c #\X) (eql c #\1))
        collect (cons y c)))

(defun applymask
  (mask value)
  (if (not mask) (return-from applymask value))
  (let ((pos (car (first mask)))
        (val (cdr (first mask))))
    (case val
      (#\0 (applymask (rest mask) value))
      (#\1 (applymask (rest mask) (logior value (ash 1 pos)))))))

(defun applymask
  (mask value)
  (unless mask (return-from applymask (list value)))
  (ecase (cdar mask)
    (#\X (append
           (applymask (rest mask) (logandc2 value (ash 1 (caar mask))))
           (applymask (rest mask) (logior value (ash 1 (caar mask))))))
    (#\1 (applymask (rest mask) (logior value (ash 1 (caar mask)))))))

(defun %setmem (memory addrs value)
  (unless addrs (return-from %setmem memory))
  (let ((addr (car addrs))
        (prev (assoc (car addrs) memory)))
    (%setmem (if prev 
               (substitute (cons addr value) prev memory)
               (append memory (cons (cons addr value) nil)))
             (rest addrs)
             value)))

(defun setmem (line memory mask)
  (%setmem
    memory
    (applymask mask (parse-integer (subseq line (1+ (position #\[ line)) (position #\] line))))
    (parse-integer (subseq line (+ (position #\= line) 2)))))

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
(format t "~a~%"
        (loop for cell in (run *demo* 'nil 'nil) sum (cdr cell)))

(defvar memory (run lines 'nil 'nil))
(format t "~a~%"
        (loop for cell in memory sum (cdr cell)))
