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

;; For more details on self-testing trace next functions:
;(trace testing-function test-case)

(defvar *tested-function* #'identity
  "Variable for binding tested functions.")

(defun testing-function (&rest input)
  "Proxy a call to the *tested-function*."
  (apply *tested-function* input))

(defun test-case (case)
  "Check one test case."
  (destructuring-bind (in => out) case
    (declare (ignore =>))
    (equal out (testing-function in))))

(defun test (function-name test-name test-cases)
  "Test function across test-cases."
  (let ((*tested-function* (symbol-function function-name)))
    (format *trace-output* "~&Running ~a~%" test-name)
    (if (notany #'null
		(mapcar #'test-case test-cases))
	(format *trace-output* "~&Test OK.~%")
	(format *trace-output* "~&Test FAILED.~%"))))

(defmacro make-test (for function-name acros test-cases &body cases)
  "Create a lambda for testing function-name acros test-cases."
  (declare (ignore for acros test-cases))
  `(function (lambda ()
      (test ',function-name
	    (format nil "~a function test" ',function-name)
	    ,@cases))))

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
  '((exp         => func let var num)
    (func        => (func-name "(" exp "," exp ")"))
    (let         => (let-op "(" var "," exp "," body))
    (body        => exp)
    (func-name   => add-func sub-func mult-func div-func)
    ;; let-op fake
    (let-op      => "let-op"))
  "Grammar for the evaluation of the calculator input.")

(defvar +skipped-lexems+
  '(" ")
  "Lexems that must be skipped.")

;;; Grammar rules macros

(defmacro define-type (type-name &key check-value-predicate
		       product-predicate)
  "Define grammar type with the predicate to check a value."
  `(setf (get ',type-name 'check-value)
         ,check-value-predicate)

  `(setf (get ',type-name 'product)
	 ,product-predicate)

  type-name)

(defmacro define-func (func-name &key name evaluating-func)
  `(progn
     (setf (get ',func-name 'check-value)
           #'(lambda (value)
               (when (stringp value)
                 (string= ,name value))))

     (setf (get ',func-name 'product)
	   #'(lambda (value)
	       (declare (ignore value))
	       ',func-name))

     (setf (get ',func-name 'evaluating-func)
           ,evaluating-func))

  func-name)

;;; Grammar rules definitions

(defun skipped-lexem-p (lexem)
  "Return T if the lexem must be skipped."
  (find lexem +skipped-lexems+ :test #'string=))

(defvar Integer.MIN_VALUE -100
  "Minimum value of the 'num' expression.")

(defvar Integer.MAX_VALUE 100
  "Maximum value of the 'num' expression.")

(defun check-integer (number-or-string)
  "Return T if number-or-string is an integer in allowed
   interval [Integer.MIN_VALUE, Integer.MAX_VALUE]."
  (let* ((value number-or-string)
	 (num
	  (typecase value
	    (number value)
	    (string
	     (multiple-value-bind (int pos)
		 (parse-integer value :junk-allowed t)
	       (when (= pos (length value))
		 int)))
	    (t nil))))
    (when num
      (and (integerp num)
		 (<= Integer.MIN_VALUE num Integer.MAX_VALUE)))))

;; Value checking lambda for the 'num' expression
(define-type num
    :check-value-predicate #'check-integer
    :product-predicate #'parse-integer)

(funcall
 (make-test for check-integer acros test-cases
   `((1 => t)
     (,(1- Integer.MIN_VALUE) => nil)
     (,(1+ Integer.MAX_VALUE) => nil)
     ("2" => t)
     ("a" => nil))))

;(funcall (get 'num 'check-value) 14)

;; Package for interning variables of the expressions
(defpackage :interview.calculator.vars)

;; Value checking lambda for the 'var' expression
(define-type var
    :check-value-predicate
  #'(lambda (value)
      (when (stringp value)
        (every #'(lambda (c)
			 (or (char<= #\A c #\Z) (char<= #\a c #\z)))
		     value)))
  :product-predicate #'(lambda (value)
			 (intern value :interview.calculator.vars)))

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

(defun make-lexer (input-string)
  "Return a closure function that will return succeeding lexems."
  (let ((s (make-string-input-stream input-string)))
    #'(lambda ()
	(read-lexem s nil))))

(defun read-lexem (&optional stream &rest read-char-args)
  "Return a succeding lexem from the stream. Result is a string."
  (concatenate 'string (apply #'read-lexem-chars
			      stream read-char-args)))

(defun read-lexem-chars (&optional stream &rest read-char-args)
  "Return a succeding lexem from the stream.
   Result is a list of characters."
  (loop for ch = (apply #'read-char stream read-char-args)
     while ch
     until (lexem-divider-p ch)
     collect ch into lexem
     finally (return
	       (if (and (lexem-divider-p ch)
			(null lexem))
		   (progn
		     (list ch))
		   (progn
		     (when ch
		       (unread-char ch stream))
		     lexem)))))

(defun lexem-divider-p (char)
  "Return T if the char is a divider lexem."
  (find char " (),"))

(defun string-to-lexems (string)
  "Convert string to lexem list."
  (loop with lexer = (make-lexer string)
     for lexem = (funcall lexer)
     while (string< "" lexem) collect lexem))

(funcall
 (make-test for string-to-lexems acros test-cases
   '(("1" => ("1"))
     ("1 2 3" => ("1" " " "2" " " "3"))
     ("1,2,3" => ("1" "," "2" "," "3"))
     ("abc" => ("abc"))
     ("a,b,-23" => ("a" "," "b" "," "-23"))
     ("a-2" => ("a-2"))
     ("  " => (" " " "))
     ("ab(cd(ef)12,22" => ("ab" "(" "cd" "(" "ef" ")" "12" "," "22")))))

(defun parse (input-string)
  "Parse input-string using grammar rules. Return a tree."
  (let* ((lexems (string-to-lexems input-string))
	 (lexems (remove-if #'skipped-lexem-p lexems)))
    (parse-with-rule 'exp lexems)))

(defun parse-with-rule (rule lexems)
  (etypecase rule
    (string (when (string= rule (first lexems))
	      (values '(t) #|(list (first lexems))|# (rest lexems))))
    (list (multiple-value-bind (p l) (parse-with-list-rule rule lexems)
	    (when p
	      (values (list p) l))))
    (symbol (let ((rv (get-rule-value (get-rule rule)))
		  (cv (get rule 'check-value))
		  (producer (get rule 'product))
		  (lexem (first lexems)))
	      (if (null rv)
		  (if cv
		      (let ((cv-result (funcall cv lexem)))
			(when cv-result
			  (values (list (funcall producer lexem))
				  (rest lexems))))
		      (error "No check-value function for ~A rule" rule))
		  (loop for v in rv
		       do (multiple-value-bind (p l) (parse-with-rule v lexems)
			    (when p
			      (return (values p l))))))))))

(defun parse-with-exp-rule (lexems)
  "Parse lexems list with the 'exp rule.
   See parse-with-rule function."
  (parse-with-rule 'exp lexems))

(funcall
 (make-test for parse-with-exp-rule acros test-cases
   `((("a") => (,(intern "a" :interview.calculator.vars)))
     (("1") => (1))
     (("add" "(" "1" "," "2" ")") => ((add-func t 1 t 2 t))))))

(defun parse-with-list-rule (list-rule lexems)
  (declare (type list list-rule))
  (unless (null list-rule)
    (multiple-value-bind (hp hl) (parse-with-rule (first list-rule) lexems)
      (if (null (rest list-rule))
	  (values hp hl)
	  (when hp
	    (multiple-value-bind (rest-p rest-l)
		(parse-with-list-rule (rest list-rule) hl)
	      (when rest-p
		(values (append hp rest-p) rest-l))))))))

(parse "add(a, div(100,-10))")

(funcall
 (make-test for parse acros test-cases
   `(("1" => (1))
     ("3" => (3))
     ("abc" => (,(intern "abc" :interview.calculator.vars)))
     ("add(1, 2)" => ((add-func t 1 t 2 t)))
     ("div( s ,  add(2,1))"
      => ((div-func t ,(intern "s" :interview.calculator.vars)
		   t (add-func t 2 t 1 t) t))))))

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
