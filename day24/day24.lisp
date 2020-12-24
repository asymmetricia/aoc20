#!/usr/bin/sbcl --script

(load "../common.lisp")
(push "./skippy-1.3.12/" asdf:*central-registry*)
(require "skippy")
(use-package '#:skippy)


(defvar *lines* (input "day24"))
; (defvar *lines* (input "day24" ".demo"))

(defstruct coord x y)

(defun directions-to-coordinates (direction-list &optional x y)
  (let ((x (or x 0))
        (y (or y 0)))
    (if (= (length direction-list) 0)
      (make-coord :x x :y y)
      (ecase (elt direction-list 0)
        (#\w (directions-to-coordinates (subseq direction-list 1) (- x 2) y))
        (#\e (directions-to-coordinates (subseq direction-list 1) (+ x 2) y))
        (#\n (ecase (elt direction-list 1)
               (#\w (directions-to-coordinates (subseq direction-list 2) (1- x) (1+ y)))
               (#\e (directions-to-coordinates (subseq direction-list 2) (1+ x) (1+ y)))))
        (#\s (ecase (elt direction-list 1)
               (#\w (directions-to-coordinates (subseq direction-list 2) (1- x) (1- y)))
               (#\e (directions-to-coordinates (subseq direction-list 2) (1+ x) (1- y)))))))))

(defun neighbors (coord)
  (list
    (make-coord :x (+ (coord-x coord) 2) :y (coord-y coord))
    (make-coord :x (- (coord-x coord) 2) :y (coord-y coord))
    (make-coord :x (1+ (coord-x coord)) :y (1+ (coord-y coord)))
    (make-coord :x (1- (coord-x coord)) :y (1+ (coord-y coord)))
    (make-coord :x (1+ (coord-x coord)) :y (1- (coord-y coord)))
    (make-coord :x (1- (coord-x coord)) :y (1- (coord-y coord)))))

(defun number-flipped (coord grid)
  (loop for coord in (neighbors coord)
        if (gethash coord grid)
        sum 1))

(defun update (grid)
  (let ((result (make-hash-table :test #'equalp)))
    (loop for coord being the hash-key of grid
          for flipped being the hash-value of grid
          for count = (number-flipped coord grid)
          do
          (if (and 
                flipped
                (or 
                  (= count 1) 
                  (= count 2))) 
            (setf (gethash coord result) t))
          (loop for coord in (neighbors coord)
                if (= (number-flipped coord grid) 2)
                do (setf (gethash coord result) t)))
    result))

(defun coord-equal (a b)
  (and
    (= (coord-x a) (coord-x b))
    (= (coord-y a) (coord-y b))))

(defvar *grid* (make-hash-table :test #'equalp))
(loop for line in *lines* 
      for coord = (directions-to-coordinates line)
      do (setf (gethash coord *grid*)
               (not (gethash coord *grid*))))

(defun bounds (grid)
  (list
    (loop for coord being the hash-key of grid
          minimize (coord-x coord))
    (loop for coord being the hash-key of grid
          maximize (coord-x coord))
    (loop for coord being the hash-key of grid
          minimize (coord-y coord))
    (loop for coord being the hash-key of grid
          maximize (coord-y coord))))

(clear)
(format t "Part 1: ~a~%" (loop for coord being the hash-key of *grid*
                               for flipped being the hash-value of *grid*
                               if flipped
                               sum 1))
(format t "~a~%" (bounds *grid*))

(let ((bounds (bounds *grid*)))
  (loop for y from (third bounds) to (fourth bounds)
        do
        (loop for x from (first bounds) to (second bounds)
              do
              (if (gethash (make-coord :x x :y y) *grid*)
                (format t "#")
                (format t ".")))
        (format t "~%")))

(defvar *pixel-size* 10)
(defun fill-pixel (canvas x y color)
  (loop for sx from (* x *pixel-size*) to (1- (* (1+ x) *pixel-size*))
        do
        (loop for sy from (* y *pixel-size*) to (1- (* (1+ y) *pixel-size*))
              do
              (setf (pixel-ref canvas sx sy) color))))

(defun grid-image (grid bounds color-table)
  (let* ((width (* *pixel-size* (1+ (- (second bounds) (first bounds)))))
         (height (* *pixel-size* (1+ (- (fourth bounds) (third bounds)))))
         (black (ensure-color (rgb-color 0 0 0) color-table))
         (red (ensure-color (rgb-color 239 51 64) color-table))
         (image (make-image :height height
                            :width width
                            :delay-time 10))
         (canvas (make-canvas :width width
                              :height height
                              :image-data (image-data image))))
    (fill (image-data image) black)
    (loop for x from (first bounds) to (second bounds)
          for sx = (- x (first bounds))
          do 
          (loop for y from (third bounds) to (fourth bounds)
                for sy = (- y (third bounds))
                if (gethash (make-coord :x x :y y) grid)
                do (fill-pixel canvas sx sy red)))
    image))
              
(defvar *final-bounds*
  (let ((grid *grid*)
        (minx 0)
        (maxx 0)
        (miny 0)
        (maxy 0))
    (loop for day from 1 to 100 do
          (setf grid (update grid))
          (let ((bounds (bounds grid)))
            (setf minx (min minx (first bounds)))
            (setf maxx (max maxx (second bounds)))
            (setf miny (min miny (third bounds)))
            (setf maxy (max maxy (fourth bounds)))))
    (list minx maxx miny maxy)))

(let* ((width  (* *pixel-size* (1+ (- (second *final-bounds*) (first *final-bounds*)))))
       (height (* *pixel-size* (1+ (- (fourth *final-bounds*) (third *final-bounds*)))))
       (data-stream (make-data-stream :height height
                                      :width width
                                      :color-table t)))
  (add-image 
    (grid-image *grid* *final-bounds* (color-table data-stream))
    data-stream)
  (loop for day from 1 to 100 do
        (setf *grid* (update *grid*))
        (add-image (grid-image *grid* *final-bounds* (color-table data-stream))
                   data-stream)
        (format t "~a~%" day))
  (output-data-stream data-stream #p"day24lisp.gif"))

; 
; (clear)
; (loop for day from 1 to 100
;       do
;       (setf *grid* (update *grid*))
;       (moveto 1 1)
;       (format t "Part 2, Day ~a: ~a~%" 
;               day
;               (loop for coord being the hash-key of *grid*
;                     for flipped being the hash-value of *grid*
;                     if flipped
;                     sum 1))
; 
;       (loop for y from (third *final-bounds*) to (fourth *final-bounds*)
;             do
;             (loop for x from (first *final-bounds*) to (second *final-bounds*)
;                   do
;                   (if (gethash (make-coord :x x :y y) *grid*)
;                     (format t "#")
;                     (format t ".")))
;             (format t "~%"))
;       (sleep 1/20))
