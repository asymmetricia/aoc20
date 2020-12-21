#!/usr/bin/sbcl --script

(load "../common.lisp")

(defvar *lines* (input "day21"))

(defstruct recipe ingredients allergens)

(defun parse-recipe (line)
  (let* ((fields (uiop:split-string line :separator " "))
         (ingredients (loop for field in fields until (equal field "(contains") collect field))
         (paren (position #\( line))
         (allergens (uiop:split-string 
                      (remove #\  (if paren (subseq line (+ paren 10) (1- (length line)))))
                      :separator ", ")))
    (make-recipe :ingredients ingredients
                 :allergens allergens)))

(let ((recipe (parse-recipe "a b c (contains x, y, z)")))
  (assert recipe)
  (assert (equal (recipe-ingredients recipe) (list "a" "b" "c")))
  (assert (equal (recipe-allergens recipe) (list "x" "y" "z"))))

(defvar *recipes* (loop for line in *lines* collect (parse-recipe line)))

(defun allergens (recipes)
  (sort 
    (copy-seq
      (reduce (lambda (a b) (union a b :test #'equal))
              (loop for recipe in recipes
                    for allergens = (recipe-allergens recipe)
                    if allergens
                    collect allergens)))
    #'string-lessp))

(let ((recipes (list
                 (parse-recipe "a b c (contains x, y, z)")
                 (parse-recipe "d e f (contains u, v, w)"))))
  (assert recipes)
  (assert (equal (allergens recipes) (list "u" "v" "w" "x" "y" "z"))))

; ingredients returns the list of all ingredients that might contain the given
; allergen
(defun ingredients (recipes allergen &optional exclude)
  (sort (copy-seq
          (set-difference
            (reduce (lambda (a b) (intersection a b :test #'equal))
                    (loop for r in recipes
                          if (member allergen (recipe-allergens r) :test #'equal)
                          collect (recipe-ingredients r)))
            exclude
            :test #'equal))
        #'string-lessp))

(let ((recipes (list
                 (parse-recipe "a b c (contains x, y, z)")
                 (parse-recipe "a b (contains x)")
                 (parse-recipe "d e f (contains u, v, w)"))))
  (assert recipes)
  (assert (equal (ingredients recipes "x") (list "a" "b")))
  (assert (equal (ingredients recipes "y") (list "a" "b" "c"))))

; solve returns a list of two item lists that map allergen name to the 
; contaminated ingredient
(defun solve (recipes &optional allergens exclude)
  (cond
    ((and (not allergens) (not exclude)) (format t "init~%")(solve recipes (allergens recipes)))
    ((not allergens) (format t "done~%") 'nil)
    (t
      (let* ((allergen (first allergens))
             (ingredients (ingredients recipes allergen exclude))
             (ingredient (first ingredients))
             (num (length ingredients)))
        (format t "checking ~a -> ~a ~a~%" allergen num ingredients)
        (if (= num 1)
          (cons (list allergen ingredients) (solve recipes (rest allergens) (cons ingredient exclude)))
          (solve recipes (append (rest allergens) (list allergen)) exclude))))))

(defvar *solution* (sort
                     (copy-seq (solve *recipes*))
                     (lambda (a b) (string-lessp (first a) (first b)))))
(defvar *unsafe* (loop for pair in *solution* append (elt pair 1)))

(format t "Part 1: ~a~%"
        (loop for recipe in *recipes* sum
              (loop for ingredient in (recipe-ingredients recipe)
                    unless (member ingredient *unsafe* :test #'equal)
                    sum 1)))

(format t "Part 2: ~{~a~^,~}~%" (loop for pair in *solution* append (elt pair 1)))
