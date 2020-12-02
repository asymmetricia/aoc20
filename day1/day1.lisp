#!/usr/bin/env -S sbcl --script

(require "asdf")
(require "split-sequence")

(defun seek (target depth input)
  (if (not input)
    (return-from seek 'nil))
  (if (= depth 0)
    (let ((found (find target input)))
      (if found (list found) 'nil))
    (let ((result (seek (- target (first input)) (- depth 1) (rest input))))
      (if result
        (concatenate 'list (list (first input)) result)
        (seek target depth (rest input))))))


(defvar depth
  (if (= (length sb-ext:*posix-argv*) 2)
    (parse-integer (elt sb-ext:*posix-argv* 1))
    1))

(defvar input (uiop:read-file-string "input"))
(setq input (string-trim '(#\newline) input))
(setq input (SPLIT-SEQUENCE:split-sequence #\newline input))
(setq input (map 'list #'parse-integer input))
(defvar result (seek 2020 depth input))
(format t "~a ~%" result)
(format t "~a ~%" (reduce #'* result))
