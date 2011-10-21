;;;; Calculator task for the Luxoft interview
;;;;
;;;; Write a calculator program in a language of your choice that evaluates
;;;; expressions in a very simple integer expression language.  The program
;;;; takes an input on the command line, computes the result, and prints it to
;;;; the console.  For example:
;;;;  
;;;; % java calculator.Main “mult(2, 2)”
;;;;  
;;;; 4
;;;;  
;;;; Few more examples:
;;;;  
;;;; +----------------------------------------+----------------------------------------+
;;;; |Input                                   |Output                                  |
;;;; |                                        |                                        |
;;;; +----------------------------------------+----------------------------------------+
;;;; |add(1, 2)                               |3                                       |
;;;; +----------------------------------------+----------------------------------------+
;;;; |add(1, mult(2, 3))                      |7                                       |
;;;; +----------------------------------------+----------------------------------------+
;;;; |mult(add(2, 2), div(9, 3))              |12                                      |
;;;; +----------------------------------------+----------------------------------------+
;;;; |let(a, 5, add(a, a))                    |10                                      |
;;;; +----------------------------------------+----------------------------------------+
;;;; |let(a, 5, let(b, mult(a, 10), add(b,    |55                                      |
;;;; |a)))                                    |                                        |
;;;; +----------------------------------------+----------------------------------------+
;;;; |let(a, let(b, 10, add(b, b)), let(b, 20,|40                                      |
;;;; |add(a, b)))                             |                                        |
;;;; +----------------------------------------+----------------------------------------+
;;;; 
;;;;  An expression consists of:
;;;;  · Numbers: integers between Integer.MIN_VALUE and Integer.MAX_VALUE
;;;;  
;;;;  · Variables: strings of characters, where each character is one of a-z, A-Z
;;;;  
;;;;  · Arithmetic functions: add, sub, mult, div, each taking two arbitrary
;;;;             expressions as arguments.  In other words, each argument may be any
;;;;             of the expressions on this list.
;;;;  
;;;;  · A “let” operator for assigning values to variables:
;;;;  
;;;;  let(<variable name>, <value>, <expression where variable is used>)
;;;;  
;;;;  As with arithmetic functions, the expression where the variable is used may be
;;;;  an arbitrary expression from this list.
;;;;  
;;;;  Please submit what you would consider testable and maintainable production
;;;;  code.  If the statement of the problem is unclear, feel free to make
;;;;  assumptions, but please state your assumptions in the solution.

(in-package :interview.calculator)

;;; Tests

(defvar +task-sample-cases+
  '(("add(1, 2)" 3)
    ("add(1, mult(2, 3))" 7)
    ("mult(add(2, 2), div(9, 3))" 12)
    ("let(a, 5, add(a, a))" 10)
    ("let(a, 5, let(b, mult(a, 10), add(b, a)))" 55)
    ("let(a, let(b, 10, add(b, b)), let(b, 20, add(a, b)))" 40))
  "Sample cases of the task.")

(defun check (case)
  "Check one test case."
  (destructuring-bind (in out) case
    (= out (calculator in))))

(defun test ()
  "Test calculator implementation."
  (format *trace-output* "~&Testing interview.calculator~%")
  (trace calculator)
  (trace check)
  (if (notany #'null
              (mapcar #'check
                      +task-sample-cases+))
      (format *trace-output* "~&Test OK.~%")
      (format *trace-output* "~&Test FAILED.~%"))
  (untrace check)
  (untrace calculator))

(eval-when (:load-toplevel)
  (test))

;;; Grammar definition

(defvar +grammar+
  '((exp         => num var func let)
    (func        => (func-name "(" exp "," exp ")"))
    (let         => (let-op "(" var "," exp "," body))
    (body        => exp)
    (func-name   => add-func sub-func mult-func div-func))
  "Grammar for the evaluation of the calculator input.")

;;; Grammar rules macros

(defmacro define-type (type-name &key check-value-predicate)
  "Define grammar type with the predicate to check a value."
  `(setf (get ',type-name 'check-value)
         ,check-value-predicate))

(defmacro define-func (func-name &key name evaluating-func)
  `(progn
     (setf (get ',func-name 'check-value)
           #'(lambda (value)
               (when (stringp value)
                 (string= ,name value))))

     (setf (get ',func-name 'evaluating-func)
           ,evaluating-func)))

;;; Grammar rules definitions

(defvar Integer.MIN_VALUE -100
  "Minimum value of the 'num' expression.")

(defvar Integer.MAX_VALUE 100
  "Maximum value of the 'num' expression.")

;; Value checking lambda for the 'num' expression
(define-type num
    :check-value-predicate
  #'(lambda (value)
      (when (numberp value)
        (and (<= Integer.MIN_VALUE value Integer.MAX_VALUE)))))

;(funcall (get 'num 'check-value) 14)

;; Value checking lambda for the 'var' expression
(define-type var
    :check-value-predicate
  #'(lambda (value)
      (when (stringp value)
        (every #'(lambda (c)
                   (or (char<= #\A c #\Z) (char<= #\a c #\z)))
               value))))

;(funcall (get 'var 'check-value) "abcS12")

(define-func add-func  :name "add"  :evaluating-func #'(lambda (a b) (+ a b)))
(define-func sub-func  :name "sub"  :evaluating-func #'(lambda (a b) (- a b)))
(define-func mult-func :name "mult" :evaluating-func #'(lambda (a b) (* a b)))
(define-func div-func  :name "div"  :evaluating-func #'(lambda (a b) (/ a b)))

;(funcall (get 'add-func 'check-value) "add")

(defvar *variables-alist* ()
  "Variables store, implemented as alist.")

(defun bind-variable (name value)
  (push (cons name value) *variables-alist*)))

(defun unbind-variable (name)
  (setf *variables-alist*
        (remove name *variables-alist* :key #'first :test #'string= :count 1)))

(defun set-variable (name value)
  (rplacd (assoc name *variables-alist* :test #'string=) value)))

(defun get-variable (name)
  (cdr (assoc name *variables-alist* :test #'string=))))

#|(let ((*variables-alist* ())
           (a))
       (bind-variable "a" 12)
       (bind-variable "b" 2)
       (bind-variable "a" 4)
       (bind-variable "b" 1)
       (set-variable "a" 9)
       (unbind-variable "b")
       (unbind-variable "a")
       (setf a (get-variable "a"))
       *variables-alist*)|#

;(define-op let-op :name "let" :evalua



;;; Grammar abstractions

(defun get-rule (name)
  "Return a grammar definition rule for the name."
  (find name +grammar+ :key #'first))

(defun get-rule-name (rule)
  "Return a name of the grammar rule."
  (first rule))

(defun get-rule-value (rule)
  "Return a value of the grammar rule. The result is a list."
  (rest (rest rule)))

;(get-rule-value (get-rule 'exp))

;;; Interface

(defun calculator (input-string)
  "Calculate an input-string. Evaluating it according to the special grammar.
   See +grammar+ variable definition."
  (declare (type string input-string))
  (length input-string))