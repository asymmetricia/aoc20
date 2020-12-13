#!/usr/bin/sbcl --script

(load "../common.lisp")

(defvar *lines* (input "day12"))

(defstruct state x y wpx wpy)

(defun left
  (state amt)
  (if (= amt 0)
    state
    (left
      (make-state 
        :x (state-x state) 
        :y (state-y state) 
        :wpx (- (state-wpy state)) 
        :wpy (state-wpx state))
      (- amt 90))))

(defun right
  (state amt)
  (if (= amt 0)
    state
    (right
      (make-state 
        :x (state-x state) 
        :y (state-y state) 
        :wpx (state-wpy state) 
        :wpy (- (state-wpx state)))
      (- amt 90))))

(defun perform
  (state cmd amt)
  (cond
    ((eql cmd #\N) (make-state :x (state-x state) :y (state-y state) :wpx (state-wpx state)         :wpy (+ (state-wpy state) amt)))
    ((eql cmd #\S) (make-state :x (state-x state) :y (state-y state) :wpx (state-wpx state)         :wpy (- (state-wpy state) amt)))
    ((eql cmd #\E) (make-state :x (state-x state) :y (state-y state) :wpx (+ (state-wpx state) amt) :wpy (state-wpy state)))
    ((eql cmd #\W) (make-state :x (state-x state) :y (state-y state) :wpx (- (state-wpx state) amt) :wpy (state-wpy state)))
    ((eql cmd #\F) (make-state 
                     :x (+ (state-x state) (* (state-wpx state) amt))
                     :y (+ (state-y state) (* (state-wpy state) amt))
                     :wpx (state-wpx state)
                     :wpy (state-wpy state)))
    ((eql cmd #\L) (left state amt))
    ((eql cmd #\R) (right state amt))))

(defvar *init-state* (make-state :x 0 :y 0 :wpx 10 :wpy 1))

(defun run (state cmds)
  (if cmds
    (run 
      (perform state (elt (first cmds) 0) (parse-integer (subseq (first cmds) 1))) 
      (rest cmds))
    state))

(defvar *result* (run *init-state* *lines*))
(format t "~a~%" *result*)
(format t "part2: ~d~%" (+ (abs (state-x *result*)) (abs (state-y *result*))))
