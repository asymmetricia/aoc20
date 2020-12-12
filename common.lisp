(defun input
  (day)
  (with-open-file (f (concatenate 'string "../" day ".input"))
    (loop for line = (read-line f nil)
          while line
          collect line)))

(defun moveto (x y) (format t "~c[~d;~dH" #\ESC y x))
(defun clear ()
  (format t "~c[2J~c[1;1H" #\ESC #\ESC))
