#!/usr/bin/sbcl --script

(setf *print-circle* t)

(defstruct game
  cups
  tail
  max
  indices)

(load "../common.lisp")

(defvar *lines* (input "day23"))

(defvar *game1* (make-game
                  :indices (make-hash-table)))

(defun game-cups-ser (game)
  (let ((head (game-cups game)))
    (cons (car head)
    (loop for cup on (cdr head)
          until (eq cup head)
          collect (car cup)))))

(defun print-game (game)
  (format nil ":CUPS ~a~%:TAIL ~a~%:MAX ~a~%:INDICES ~{~%~T~a~}"
          (game-cups game) 
          (game-tail game)
          (game-max game)
          (loop for k being the hash-key of (game-indices game)
                for v being the hash-value of (game-indices game)
                collect (format nil ":~a ~a" k v))))

(defun add-cup (game cup)
  (if (game-cups game)
    (block nil
           (setf (cdr (game-tail game)) (cons cup (cdr (game-tail game))))
           (setf (game-tail game) (cdr (game-tail game))))
    (block nil
           (setf (game-cups game) (cons cup (game-cups game)))
           (setf (cdr (game-cups game)) (game-cups game))
           (setf (game-tail game) (game-cups game))))
  (setf (gethash cup (game-indices game)) (game-tail game))
  (if (or (not (game-max game))
          (> cup (game-max game)))
    (setf (game-max game) cup)))

(loop for c across (first *lines*) 
      do (add-cup *game1* (parse-integer (string c))))

(defun target (target game)
  (if (< target 1)
    (target (game-max game) game)
    (if (or (= target (cadr (game-cups game)))
            (= target (caddr (game-cups game)))
            (= target (cadddr (game-cups game))))
      (target (1- target) game)
      target)))

; (defun move (game)
;   (let* ((cups (game-cups game))
;          (pick (cdr cups))
;          (target (gethash (target (1- (car cups)) game) (game-indices game))))
;     (setf (cdr cups) (cddddr cups))
;     (setf (cdddr pick) (cdr target))
;     (setf (cdr target) pick)
;     (setf (game-cups game) (cdr cups))))

(defun move (game)
  (let* ((head (game-cups game))
         (current (car head))
         (targetLabel (target (1- current) game))
         (target (gethash targetLabel (game-indices game)))
         (pick (cdr head)))
    (setf (cdr head) (cdddr pick))     ; link first cup to fifth cup
    (setf (cdddr pick) (cdr target))   ; link tail of picked cups to cup after target
    (setf (cdr target) pick)           ; link target cup to head of picked cups
    (setf (game-cups game) (cdr head)) ; link head to next in line
    ))

(loop for i from 0 to 99 do
      (move *game1*))

(let ((one (gethash 1 (game-indices *game1*))))
  (format t "Part 1: ~{~a~}~%"
          (loop for cup on (cdr one)
                until (eq cup one)
                collect (car cup))))

(defvar *game2* (make-game 
                  :indices (make-hash-table)))

(loop for c across (first *lines*) 
      do (add-cup *game2* (parse-integer (string c))))
(loop for c from 10 to 1000000 do (add-cup *game2* c))

(loop for round from 1 to 10000000 do (move *game2*))

(let ((one (gethash 1 (game-indices *game2*))))
  (format t "Part 2: ~a~%" (* (cadr one) (caddr one))))
