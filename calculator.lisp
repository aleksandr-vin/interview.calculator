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

;;; Testing framework

(defvar *tested-function* #'identity
  "Variable for binding tested functions.")

(defun testing-function (&rest input)
  "Proxy a call to the *tested-function*."
  (apply *tested-function* input))

(defun test-case (case)
  "Check one test case."
  (destructuring-bind (in => out) case
    (equal out (testing-function in))))

(trace testing-function
       test-case)

(defun test (function-name test-name test-cases)
  "Test function across test-cases."
  (let ((*tested-function* (symbol-function function-name)))
    (format *trace-output* "~&Running ~a~%" test-name)
    (if (notany #'null
		(mapcar #'test-case test-cases))
	(format *trace-output* "~&Test OK.~%")
	(format *trace-output* "~&Test FAILED.~%"))))

(defmacro make-test (for function-name acros test-cases &body test-cases)
  "Create a lambda for testing function-name acros test-cases."
  `(function (lambda ()
      (test ',function-name
	    (format nil "~a function test" ',function-name)
	    ,@test-cases))))

(funcall
 (make-test for string acros test-cases
   '((A => "A")
     (Q => "Q"))))

(defmacro define-test-cases (name documentation &body test-cases)
  "Store test-cases in global variable with name."
  `(defvar ,name ',test-cases ,documentation))

(define-test-cases +task-sample-cases+
    "Sample cases of the task."
  ("add(1, 2)" => 3)
  ("add(1, mult(2, 3))" => 7)
  ("mult(add(2, 2), div(9, 3))" => 12)
  ("let(a, 5, add(a, a))" => 10)
  ("let(a, 5, let(b, mult(a, 10), add(b, a)))" => 55)
  ("let(a, let(b, 10, add(b, b)), let(b, 20, add(a, b)))" => 40))

;;; Grammar definition

(defvar +grammar+
  '((exp         => num var func let)
    (func        => (func-name "(" exp "," exp ")"))
    (let         => (let-op "(" var "," exp "," body))
    (body        => exp)
    (func-name   => add-func sub-func mult-func div-func))
  "Grammar for the evaluation of the calculator input.")

(defvar +lexems+
  '(" " "(" ")" ","
    num var
    add-func sub-func mult-func div-func
    let-op)
  "Lexems of the grammar.")

(defvar +skipped-lexems+
  '(" ")
  "Lexems that must be skipped.")

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
  #'check-integer)

(defun check-integer (number-or-string)
  "Return T if number-or-string is an integer in allowed
   interval [Integer.MIN_VALUE, Integer.MAX_VALUE]."
  (let* ((value number-or-string)
	 (num
	  (typecase value
	    (number value)
	    (string (parse-integer value :junk-allowed t))
	    (t nil))))
    (when num
      (and (<= Integer.MIN_VALUE num Integer.MAX_VALUE)))))

(funcall
 (make-test for check-integer acros test-cases
   `((1 => t)
     (,(1- Integer.MIN_VALUE) => nil)
     (,(1+ Integer.MAX_VALUE) => nil)
     ("2" => t)
     ("a" => nil))))

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
  (push (cons name value) *variables-alist*))

(defun unbind-variable (name)
  (setf *variables-alist*
        (remove name *variables-alist* :key #'first :test #'string= :count 1)))

(defun set-variable (name value)
  (rplacd (assoc name *variables-alist* :test #'string=) value))

(defun get-variable (name)
  (cdr (assoc name *variables-alist* :test #'string=)))

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

(defun parse (input-string)
  "Parse input-string using grammar rules. Return a tree.")

(defun make-lexer (input-string)
  "Return a closure function that will return succeeding lexems."
  (let ((s (make-string-input-stream input-string)))
    #'(lambda ()
	(read-lexem s nil))))

(defun read-lexem (&optional stream &rest read-char-args)
					;(apply #'read-char stream read-char-args))
  (loop for ch = (apply #'read-char stream read-char-args)
     while ch
     collect ch into lexem
     while (lexemp (concatenate 'string lexem))
     finally (return lexem)))

(defun lexemp (lexem?)
  "Return T if the string is a lexem."
  (when (find-if #'(lambda (l)
	       (cond ((stringp l) (string= l lexem?))
		     ((symbolp l)
		      (let ((check (get l 'check-value)))
			(when check
			  (funcall check lexem?))))
		     (t nil)))
	   +lexems+)
    t))

(funcall
 (make-test for lexemp acros test-cases
   `(("a" => t)
     ("a b" => nil)
     ("(" => t)
     (" " => t)
     (")" => t)
     (,(format nil "~S" (1- Integer.MAX_VALUE)) => t)
     (,(format nil "~S" (1+ Integer.MAX_VALUE)) => nil))))

(with-input-from-string (s "abcd")
  (read-lexem s nil))

(defvar x (make-lexer "abcd"))
(funcall x)

(defun string-to-lexems (string)
  "Convert string to lexem list."
  (do* ((lexer (make-lexer string))
	(lexem t (funcall lexer))
	(lexems nil (cons lexem lexems)))
       ((not lexem) (nreverse (rest lexems)))))

; Alternative implementation via LOOP macro
(defun string-to-lexems (string)
  "Convert string to lexem list."
  (loop with lexer = (make-lexer string)
     for lexem = (funcall lexer)
     while lexem collect lexem))

(string-to-lexems "abcd")

(funcall
 (make-test for parse acros test-cases
   '(("1" => 1)
     ("3" => 3)
     ("add(1, 2)" => (add 1 2)))))

;;; Interface

(defun calculator (input-string)
  "Calculate an input-string. Evaluating it according to the special grammar.
   See +grammar+ variable definition."
  (declare (type string input-string))
  (length input-string))

;;; Autotests

(eval-when (:load-toplevel)
  (funcall
   (make-test for calculator
       acros test-cases
     +task-sample-cases+)))
