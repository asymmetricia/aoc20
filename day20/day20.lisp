#!/usr/bin/sbcl --script

(load "../common.lisp")
(load "cells.lisp")

(defvar *lines* (input "day20"))

(defun chunk-tiles (lines &optional accum)
  (unless lines (return-from chunk-tiles 'nil))
  (let ((line (first lines)))
    (if (equal line "")
      (cons accum (chunk-tiles (rest lines)))
      (chunk-tiles (rest lines) (append accum (list line))))))

(defstruct tile id cells)

(defun parse-tile (lines)
  (let ((tile (make-tile 
                :id (parse-integer (subseq (first lines) 5 (1- (length (first lines)))))
                :cells (make-array (list (length (elt lines 1)) (1- (length lines)))))))
    (loop for line in (rest lines)
          for y from 0
          do (loop for c across line
                   for x from 0
                   do (setf (aref (tile-cells tile) x y) (eql c #\#))))
    tile))
          
(defvar *tiles* (loop for chunk in (chunk-tiles *lines*) collect (parse-tile chunk)))

(defmacro transform-tile (tile &rest body)
  `(make-tile :id (tile-id ,tile)
              :cells (transform-cells (tile-cells ,tile) ,@body)))


(defun tile-flip-x (tile)
  (transform-tile tile
                  (setf (aref new-cells (- dimx 1 x) y) cell)))

; given 0..9
; (2,1) -> (8,  2) -> (7,   8) -> (1,   7) -> (2,   1) 
; (x,y) -> (9-y,x) -> (9-y, x) -> (9-y, x) -> (9-y, x)
(defun tile-rotate (tile)
  (transform-tile tile
                  (setf (aref new-cells (- dimy 1 y) x) cell)))

(defun tile-equal-p (a b)
  (each-cell (tile-cells a) (unless (eql cell (aref (tile-cells b) x y)) (return-from tile-equal-p 'nil)))
  t)

(loop for tile in *tiles* do
      (assert (tile-equal-p tile
                            (tile-rotate (tile-rotate (tile-rotate (tile-rotate tile)))))))

(defun print-at (tile sx sy)
  (each-cell (tile-cells tile)
             (moveto (+ sx x) (+ sy y))
             (format t "~a" (if cell #\# #\.))))

(defun fits-p (a b a-side)
  (let* ((cells-a (tile-cells a))
         (cells-b (tile-cells b))
         (dimx (array-dimension cells-a 0))
         (dimy (array-dimension cells-a 1)))
    (ecase a-side
      ('east (loop for y from 0 to (1- dimy)
                   for cell-a = (aref cells-a (1- dimx) y)
                   for cell-b = (aref cells-b 0 y)
                   do 
                   (unless (eql cell-a cell-b) (return-from fits-p 'nil))))
      ('west (return-from fits-p (fits-p b a 'east)))
      ('north (loop for x from 0 to (1- dimx)
                   for cell-a = (aref cells-a x 0)
                   for cell-b = (aref cells-b x (1- dimy))
                   do 
                   (unless (eql cell-a cell-b) (return-from fits-p 'nil))))
      ('south (return-from fits-p (fits-p b a 'north)))
      ))
  t)

(assert
  (fits-p
    (parse-tile '("Tile 1:" ".#" ".."))
    (parse-tile '("Tile 2:" "#." ".."))
    'east))

(assert
  (fits-p
    (parse-tile '("Tile 1:" ".#" ".."))
    (parse-tile '("Tile 2:" "#." ".#"))
    'north))

(assert (not (fits-p
    (parse-tile '("Tile 1:" 
                  ".#." 
                  "#.#"
                  ".#."))
    (parse-tile '("Tile 2:" 
                  "#.#" 
                  "..."
                  "#.#"))
    'north)))

(defun tile-try (target candidate direction &optional transforms &key print-coords)
  (let* ((transforms (or transforms (list 'nil
                                          'tile-rotate 
                                          'tile-rotate 
                                          'tile-rotate 
                                          'tile-flip-x 
                                          'tile-rotate 
                                          'tile-rotate 
                                          'tile-rotate)))
         (next-transforms (rest transforms))
         (transformed (if (first transforms) (funcall (first transforms) candidate) candidate)))
    (if print-coords (print-at transformed (car print-coords) (cdr print-coords)))
    (cond
      ((fits-p target transformed direction) transformed)
      (next-transforms (tile-try target transformed direction next-transforms)))))

(assert
  (tile-try
    (parse-tile '("Tile 1:" ".#" ".."))
    (parse-tile '("Tile 2:" "#." ".."))
    'east))

(defun tile-find (target tiles direction)
  (loop for tile in tiles
        unless (equal (tile-id target) (tile-id tile))
        do (let ((match (tile-try target tile direction)))
             (if match (return-from tile-find match)))))

(let ((tiles (list (parse-tile '("Tile 1:" 
                                 ".#" 
                                 ".."))
                   (parse-tile '("Tile 2:" 
                                 "#." 
                                 "..")))))
  (assert (tile-find (elt tiles 0) tiles 'east)))

(defun tile-find-top-left (tiles &optional tile direction)
  (let* ((tile (or tile (elt tiles 0)))
         (direction (or direction 'west))
         (match (tile-find tile tiles direction)))
    (cond
      (match (tile-find-top-left tiles match direction))
      ((eql direction 'north) tile)
      (t (tile-find-top-left tiles tile 'north)))))

(defun arrange-row (left tiles)
  (let ((next (tile-find left tiles 'east)))
    (cons left (and next (arrange-row next tiles)))))

(defun arrange (top tiles)
  (let ((row (arrange-row top tiles))
        (next (tile-find top tiles 'south)))
    (cons row (and next (arrange next tiles)))))

(defvar *top-left* (tile-find-top-left *tiles*))
(assert *top-left*)

(defvar *grid* (arrange *top-left* *tiles*))

(format t "Part 1: ~a~%" (*
                           (tile-id (first (first *grid*)))
                           (tile-id (car (last (first *grid*))))
                           (tile-id (first (car (last *grid*))))
                           (tile-id (car (last (car (last *grid*)))))))

(defvar *monster* '(
                    (0 0) (1 1)
                    (4 1) (5 0) (6 0) (7 1)
                    (10 1) (11 0) (12 0) (13 1)
                    (16 1) (17 0) (18 0) (18 -1) (19 0)))

(defvar *image* (make-array (list
                              (* (length (first *grid*))
                                 (- 
                                   (array-dimension (tile-cells *top-left*) 0)
                                   2))
                              (* (length *grid*)
                                 (-
                                   (array-dimension (tile-cells *top-left*) 1)
                                   2)))))

(loop for row in *grid*
      for gy from 0
      do (loop for tile in row
               for gx from 0
               for cells = (tile-cells tile)
               for dimx = (array-dimension cells 0)
               for dimy = (array-dimension cells 1)
               do (loop for x from 1 to (- dimx 2)
                        for ix = (+ (* (- dimx 2) gx) (1- x))
                        do (loop for y from 1 to (- dimy 2)
                                 for iy = (+ (* (- dimy 2) gy) (1- y))
                                 do (setf (aref *image* ix iy) (aref cells x y))))))

(defun monster-p (image x y)
  (let ((dimx (array-dimension image 0))
        (dimy (array-dimension image 1)))
    (every #'identity
           (loop for cell in *monster*
                 for mx = (+ x (elt cell 0))
                 for my = (+ y (elt cell 1))
                 collect (and
                           (>= mx 0)
                           (>= my 0)
                           (< mx dimx)
                           (< my dimy)
                           (aref image mx my))))))

(defun count-monsters (image)
  (let ((dimx (array-dimension image 0))
        (dimy (array-dimension image 1)))
    (loop for y from 0 to (- dimy 2)
          sum (loop for x from 0 to (1- dimx)
                    if (monster-p image x y)
                    sum 1))))

(defvar *demo-monster* 
  (parse-tile '("Tile 1:"
                "..................#."
                "#....##....##....###"
                ".#..#..#..#..#..#...")))

(assert (monster-p
          (tile-cells *demo-monster*)
          0 1))
(assert (= 1 (count-monsters (tile-cells *demo-monster*))))

(defvar *demo* (parse-tile
                 '("Tile 1:"
                   ".#.#..#.##...#.##..#####"
                   "###....#.#....#..#......"
                   "##.##.###.#.#..######..."
                   "###.#####...#.#####.#..#"
                   "##.#....#.##.####...#.##"
                   "...########.#....#####.#"
                   "....#..#...##..#.#.###.."
                   ".####...#..#.....#......"
                   "#..#.##..#..###.#.##...."
                   "#.####..#.####.#.#.###.."
                   "###.#.#...#.######.#..##"
                   "#.####....##..########.#"
                   "##..##.#...#...#.#.#.#.."
                   "...#..#..#.#.##..###.###"
                   ".#.#....#.##.#...###.##."
                   "###.#...#..#.##.######.."
                   ".#.#.###.##.##.#..#.##.."
                   ".####.###.#...###.#..#.#"
                   "..#.#..#..#.#.#.####.###"
                   "#..####...#.#.#.###.###."
                   "#####..#####...###....##"
                   "#.##..#..#...#..####...#"
                   ".#.###..##..##..####.##."
                   "...###...##...#...#..###")))

(defvar *demo-oriented* (cells-rotate (cells-rotate (cells-rotate (cells-flip-x (tile-cells *demo*))))))
(assert (= 2 (count-monsters *demo-oriented*)))

(defvar *num-monsters*
  (loop for xform in (list
                       *image*
                       (cells-rotate *image*)
                       (cells-rotate (cells-rotate *image*))
                       (cells-rotate (cells-rotate (cells-rotate *image*)))
                       (cells-flip-x *image*)
                       (cells-rotate (cells-flip-x *image*))
                       (cells-rotate (cells-rotate (cells-flip-x *image*)))
                       (cells-rotate (cells-rotate (cells-rotate (cells-flip-x *image*)))))
        for monsters = (count-monsters xform)
        if (> monsters 0)
        sum monsters))

(defvar *num-active* (loop for x from 0 to (1- (array-dimension *image* 0))
                           sum (loop for y from 0 to (1- (array-dimension *image* 1))
                                     if (aref *image* x y)
                                     sum 1)))

(format t "Part 2: ~a~%" (- *num-active* (* *num-monsters* (length *monster*))))
