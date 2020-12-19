#!/usr/bin/sbcl --script

(load "../common.lisp")
(defvar *lines* (input "day16"))

; given rule, a string 
(defun parse-rule (rule)
  (let* ((colon (position #\: rule))
         (name (subseq rule 0 colon))
         (dash1 (position #\- rule))
         (min1 (parse-integer (subseq rule (+ 2 colon) dash1)))
         (preOrSpace (position #\  rule :start dash1))
         (max1 (parse-integer (subseq rule (+ 1 dash1) preOrSpace)))
         (dash2 (position #\- rule :start preOrSpace))
         (min2 (parse-integer (subseq rule (+ 4 preOrSpace) dash2)))
         (max2 (parse-integer (subseq rule (+ 1 dash2)))))
    (list
      name
      (lambda (x) (or
                    (and (>= x min1) (<= x max1))
                    (and (>= x min2) (<= x max2)))))))

(defun error-value (rules ticket)
  (loop for field in ticket sum
        (if (some #'identity
                  (loop for rule in rules
                        collect (funcall (elt rule 1) field)))
          0
          field)))

(defvar *rules* (loop for line in *lines* 
                      until (equal line "")
                      collect (parse-rule line)))

(format t "Rules: ~a~%" (length *rules*))

(defun parse-ticket (ticket) (loop 
                               for value in (uiop:split-string ticket :separator ",")
                               collect (parse-integer value)))

(defvar *mine* (parse-ticket (elt *lines* (+ 2 (length *rules*)))))
(defvar *nearby* (loop for ticket in (subseq *lines* (+ 5 (length *rules*)))
                       collect (parse-ticket ticket)))

(format t "Nearby Tickets: ~a~%" (length *nearby*))

(format t "Part 1: ~a~%" (loop 
                       for ticket in *nearby*
                       sum (error-value *rules* ticket)))

(defvar *valids* (loop for ticket in *nearby*
                       if (= (error-value *rules* ticket) 0)
                       collect ticket))
(format t "Valid Tickets: ~a~%" (length *valids*))

(defun map-rule (rule valids &optional available)
  (let* ((available (or available (loop for field in (elt *valids* 0) collect t))))
    (reduce
      (lambda (a b) 
        (loop for va in a
              for vb in b
              for avail in available
              collect (and va vb avail)))
      (loop for ticket in valids
            collect
            (loop for field in ticket
                  collect (funcall (elt rule 1) field))))))

(defun map-rules (rules valids &optional available)
  (unless rules (return-from map-rules nil))
  (let* ((available (or available (loop for field in (elt *valids* 0) collect t)))
         (mappable (loop for rule in rules
                         for mapping = (map-rule rule valids available)
                         if (eq (count t mapping) 1)
                         return (list rule (position t mapping))))
         (fieldNo (elt mappable 1))
         (unmapped (remove (elt mappable 0) rules))
         (newavail (substitute nil t available :start fieldNo :end (1+ fieldNo))))
    (append
      (list mappable)
      (map-rules unmapped valids newavail))))

(defvar *mapping* (map-rules *rules* *valids*))
(format t "Part 2: ~a~%" (reduce #'*
                         (loop for field in *mine*
                               for i from 0
                               for rule = (elt (find-if (lambda (mapping) (= i (elt mapping 1))) *mapping*) 0)
                               if (uiop:string-prefix-p "departure " (elt rule 0))
                               collect field)))
