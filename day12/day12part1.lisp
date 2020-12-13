#!/usr/bin/sbcl --script

(load "../common.lisp")

(defvar *lines* (input "day12"))

(defstruct state x y facing)

(defun left
  (state amt)
  (if (= amt 0)
    state
    (left
      (cond
        ((eql (state-facing state) #\N) (make-state :x (state-x state) :y (state-y state) :facing #\W))
        ((eql (state-facing state) #\W) (make-state :x (state-x state) :y (state-y state) :facing #\S))
        ((eql (state-facing state) #\S) (make-state :x (state-x state) :y (state-y state) :facing #\E))
        ((eql (state-facing state) #\E) (make-state :x (state-x state) :y (state-y state) :facing #\N)))
      (- amt 90))))

(defun right
  (state amt)
  (if (= amt 0)
    state
    (right
      (cond
        ((eql (state-facing state) #\N) (make-state :x (state-x state) :y (state-y state) :facing #\E))
        ((eql (state-facing state) #\W) (make-state :x (state-x state) :y (state-y state) :facing #\N))
        ((eql (state-facing state) #\S) (make-state :x (state-x state) :y (state-y state) :facing #\W))
        ((eql (state-facing state) #\E) (make-state :x (state-x state) :y (state-y state) :facing #\S)))
      (- amt 90))))

(defun perform
  (state cmd amt)
  (cond
    ((eql cmd #\N) (make-state :x (state-x state)         :y (+ (state-y state) amt) :facing (state-facing state)))
    ((eql cmd #\S) (make-state :x (state-x state)         :y (- (state-y state) amt) :facing (state-facing state)))
    ((eql cmd #\E) (make-state :x (+ (state-x state) amt) :y (state-y state)         :facing (state-facing state)))
    ((eql cmd #\W) (make-state :x (- (state-x state) amt) :y (state-y state)         :facing (state-facing state)))
    ((eql cmd #\F) (perform state (state-facing state) amt))
    ((eql cmd #\L) (left state amt))
    ((eql cmd #\R) (right state amt))))

(defvar *init-state* (make-state :x 0 :y 0 :facing #\E))

(defun run (state cmds)
  (if cmds
    (run 
      (perform state (elt (first cmds) 0) (parse-integer (subseq (first cmds) 1))) 
      (rest cmds))
    state))

(defvar *result* (run *init-state* *lines*))
(format t "~a~%" *result*)
(format t "part1: ~d~%" (+ (abs (state-x *result*)) (abs (state-y *result*))))
