#!/usr/bin/sbcl --script

(load "../common.lisp")

(defvar *lines* (input "day22"))
(defvar *p1* (loop for line in (rest *lines*)
                   until (equal line "")
                   collect (parse-integer line)))

(defvar *p2* (loop for line in (nthcdr (+ (length *p1*) 3) *lines*)
                   collect (parse-integer line)))

(defun play (p1 p2)
  (cond
    ((not p1) p2)
    ((not p2) p1)
    ((> (first p1) (first p2))
     (play (append (rest p1) (list (first p1) (first p2))) (rest p2)))
    (t
     (play (rest p1) (append (rest p2) (list (first p2) (first p1)))))))

(defun score (deck)
  (loop for card in deck
        for i downfrom (length deck)
        sum (* i card)))

(format t "Part 1: ~a~%" (score (play *p1* *p2*)))

(defun recursive-play (p1 p2 &optional states)
  (if (find (list p1 p2) states :test #'equal) (return-from recursive-play (values p1 t)))
  (unless p1 (return-from recursive-play (values p2 'nil)))
  (unless p2 (return-from recursive-play (values p1 t)))
  (let* ((c1 (first p1))
         (c2 (first p2))
         (result
           (if (and (< c1 (length p1))
                    (< c2 (length p2)))
             (multiple-value-bind (deck p1win)
               (recursive-play (copy-seq (subseq (rest p1) 0 c1))
                               (copy-seq (subseq (rest p2) 0 c2)))
               p1win)
             (> c1 c2))))
    (if result
      (recursive-play (append (rest p1) (list c1 c2))
                      (rest p2)
                      (append states (list (list p1 p2))))
      (recursive-play (rest p1)
                      (append (rest p2) (list c2 c1))
                      (append states (list (list p1 p2)))))))

(format t "Part 2: ~a~%" (score (recursive-play *p1* *p2*)))
