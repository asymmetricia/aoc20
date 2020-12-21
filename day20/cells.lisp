(defmacro transform-cells (cells &rest body)
  `(let* ((new-cells (make-array (array-dimensions ,cells))))
     (each-cell ,cells ,@body)
     new-cells))

(defmacro each-cell (cells &rest body)
  `(let* ((dimx (array-dimension ,cells 0))
          (dimy (array-dimension ,cells 1)))
     (loop for x from 0 to (1- dimx)
           do (loop for y from 0 to (1- dimy)
                    for cell = (aref ,cells x y)
                    do (progn ,@body)))))

(defun cells-flip-x (cells)
  (transform-cells cells
                   (setf (aref new-cells (- dimx 1 x) y) cell)))

(defun cells-rotate (cells)
  (transform-cells cells (setf (aref new-cells (- dimy 1 y) x) cell)))

(defun print-cells (cells)
  (loop for y from 0 to (1- (array-dimension cells 1))
        do (loop for x from 0 to (1- (array-dimension cells 0))
                 do (format t "~a" (if (aref cells x y) #\# #\.)))
        (format t "~%")))
