#!/usr/bin/sbcl --script

(require "asdf")
(require "split-sequence")

(defvar passports '())
(defvar passport (gentemp))

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
                     (setf passport (gentemp))))
              ((string= (subseq pair 0 4) "cid:") ())
              (t (setf (get passport (intern (string-upcase (subseq pair 0 3)))) (subseq pair 4))))))

(setf passports (cons passport passports))

(defun validateNum
  (passport indicator min max)
  (if (get passport indicator) () (return-from validateNum 'nil))
  (let ((byr (parse-integer (get passport indicator))))
    (and
      byr
      (>= byr min)
      (<= byr max))))

(defun validateHgt
  (passport)
  (let ((hgt (get passport 'hgt)))
    (if (not hgt) (return-from validateHgt))
    (if (< (length hgt) 3) (return-from validateHgt))
    (let ((suffix (subseq hgt (- (length hgt) 2)))
          (value (parse-integer (subseq hgt 0 (- (length hgt) 2)))))
      (cond
        ((string= suffix "cm") (and (>= value 150) (<= value 193)))
        ((string= suffix "in") (and (>= value 59) (<= value 76)))))))

(defun validateHcl
  (passport)
  (let ((hcl (get passport 'hcl)))
    (and
      hcl
      (eq (char hcl 0) #\#)
      (= (length hcl) 7)
      (every #'identity
              (loop for char across (subseq hcl 1)
                    collect (find char "0123456789abcdef"))))))

(defun validateEcl
  (passport)
  (let ((ecl (get passport 'ecl)))
    (and
      ecl
      (or
        (string= ecl "amb")
        (string= ecl "blu")
        (string= ecl "brn")
        (string= ecl "gry")
        (string= ecl "grn")
        (string= ecl "hzl")
        (string= ecl "oth")))))

(defun validatePid
  (passport)
  (let ((pid (get passport 'pid)))
    (= (length pid) 9)))

(defun validate
  (passport)
  (and
    (= (length (symbol-plist passport)) 14)
    (validateNum passport 'byr 1920 2002)
    (validateNum passport 'iyr 2010 2020)
    (validateNum passport 'eyr 2020 2030)
    (validateHgt passport)
    (validateHcl passport)
    (validateEcl passport)
    (validatePid passport)))

(format t "~a ~%"
  (loop for passport in passports 
        sum (if (validate passport) 1 0)))
