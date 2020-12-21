(require "asdf")
(require "uiop")

(defun input
  (day &optional ext)
  (with-open-file (f (concatenate 'string "../" day (or ext ".input")))
    (loop for line = (read-line f nil)
          while line
          collect line)))

(defun moveto (x y) (format t "~c[~d;~dH" #\ESC y x))
(defun clear (&optional to-end)
  (if to-end
    (format t "~c[J~c[1G" #\ESC #\ESC)
    (format t "~c[2J~c[1;1H" #\ESC #\ESC)))
(defun clear-line ()
  (format t "~c[2K~c[1G" #\ESC #\ESC))
