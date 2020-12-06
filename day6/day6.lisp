#!/usr/bin/sbcl --script

(defvar lines
  (with-open-file (f "part1/input")
    (loop for line = (read-line f nil)
          while line
          collect line)))

(defvar idx 0)
(defvar groups (list (gensym "group")))

(loop for line in lines do
      (if (string= line "")
        (block nil
          (setf idx (1+ idx))
          (setf groups (append groups (list (gensym "group")))))
        (block nil
          (setf (get (elt groups idx) 'count) (1+ (get (elt groups idx) 'count 0)))
          (loop for c across line do
                (setf (get (elt groups idx) c) (1+ (get (elt groups idx) c 0)))))))

(format t "part 1: ~a ~%"
  (loop for group in groups
        sum (1- (/ (length (symbol-plist group)) 2))))

(format t "part 2: ~a ~%"
  (loop for group in groups 
        sum (loop for (key value) on (symbol-plist group) by #'cddr
                  with count = (get group 'count)
                  when (eq value count)
                  unless (eq key 'count)
                  sum 1)))
