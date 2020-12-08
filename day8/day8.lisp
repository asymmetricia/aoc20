#!/usr/bin/sbcl --script

(require "asdf")
(require "uiop")

(defstruct op 
  (op "" :type string)
  (val 0 :type number))

(defvar *program*
  (with-open-file (f "../day8.input")
    (loop for line = (read-line f nil)
          for words = (uiop:split-string line)
          while line
          collect (make-op :op (first words) :val (parse-integer (second words))))))

(defvar seen (make-hash-table))

(defstruct patch addr op)

(defun run (program accum ptr seen &optional patch)
  (if (assoc ptr seen)
    (return-from run (values accum 'loop)))
  (if (>= ptr (length program))
    (return-from run (values accum nil)))
  (let ((instruction (if (and patch (eql ptr (patch-addr patch))) (patch-op patch) (elt program ptr))))
    (let ((op  (op-op instruction))
          (val (op-val instruction)))
      (cond
        ((equal op "nop") (run program accum (1+ ptr) (acons ptr t seen) patch))
        ((equal op "jmp") (run program accum (+ ptr val) (acons ptr t seen) patch))
        ((equal op "acc") (run program (+ accum val) (1+ ptr) (acons ptr t seen) patch))
        (t (format t "bad op ~a ~%" op))))))

(format t "~a~%" (run *program* 0 0 nil))

(loop for y from 0 below (length *program*)
      for op = (elt *program* y)
      for opcode = (op-op op)
      for val = (op-val op)
      for patch = (make-patch :addr y :op (make-op :op (cond
                                                     ((equal opcode "nop") "jmp") 
                                                     ((equal opcode "jmp") "nop")
                                                     (t opcode))
                                                   :val val))
      do (multiple-value-bind 
           (accum reason)
           (run *program* 0 0 nil patch)
           (unless reason (format t "~a~%" accum))))
