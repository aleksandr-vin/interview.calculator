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

(defmacro make-test (for function-name across test-cases &body cases)
  "Create a lambda for testing function-name across test-cases."
  (declare (ignore for across test-cases))
  `(function (lambda ()
      (test ',function-name
	    (format nil "~a function test" ',function-name)
	    ,@cases))))

(funcall
 (make-test for string across test-cases
   '((A => "A")
     (Q => "Q"))))

(defmacro define-test-cases (name documentation &body test-cases)
  "Store test-cases in global variable with name."
  `(defvar ,name ',test-cases ,documentation))

;;; Grammar definition

(defvar +grammar+
  '((exp         => func let var num)
    (func        => (func-name "(" exp "," exp ")"))
    (let         => (let-op "(" var "," exp "," body ")"))
    (body        => exp)
    (func-name   => add-func sub-func mult-func div-func)
    (let-op      => "let"))
  "Calculator expression definition grammar.")

(defvar +skipped-lexems+
  '(" ")
  "Lexems that must be skipped.")

;;; Grammar rules macros

(defmacro define-type (type-name &key check-value-predicate
		       product-function)
  "Define grammar type with the predicate to check a value."
  `(eval-when (:load-toplevel :execute)
     (setf (get ',type-name 'check-value)
         ,check-value-predicate)
     (setf (get ',type-name 'product)
	 ,product-function)
     ,type-name))

(defmacro define-func (func-name &key name evaluating-func)
  `(eval-when (:load-toplevel :execute)
     (setf (get ',func-name 'check-value)
           #'(lambda (value)
               (when (stringp value)
                 (string= ,name value))))
     (setf (get ',func-name 'product)
	   #'(lambda (value)
	       (declare (ignore value))
	       (intern ',func-name :interview.calculator.funcs)))
     (export (intern ',func-name :interview.calculator.funcs)
	     :interview.calculator.funcs)
     (setf (symbol-function (intern ',func-name :interview.calculator.funcs))
	   ,evaluating-func)
     ',func-name))

(defpackage :interview.calculator.funcs)

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
    :product-function #'parse-integer)

(funcall
 (make-test for check-integer across test-cases
   `((1 => t)
     (,(1- Integer.MIN_VALUE) => nil)
     (,(1+ Integer.MAX_VALUE) => nil)
     ("2" => t)
     ("a" => nil))))

(defun check-var-name (string)
  "Return T if string is a valid name for the calculator variable."
  (when (stringp string)
        (every #'(lambda (c)
			 (or (char<= #\A c #\Z) (char<= #\a c #\z)))
		     string)))

;; Package for interning variables of the expressions
(defpackage :interview.calculator.vars)

;; Value checking lambda for the 'var' expression
(define-type var
    :check-value-predicate #'check-var-name
  :product-function #'(lambda (value)
			 (intern value :interview.calculator.vars)))

(define-func add-func  :name "add"  :evaluating-func #'(lambda (a b) (+ a b)))
(define-func sub-func  :name "sub"  :evaluating-func #'(lambda (a b) (- a b)))
(define-func mult-func :name "mult" :evaluating-func #'(lambda (a b) (* a b)))
(define-func div-func  :name "div"  :evaluating-func #'(lambda (a b) (/ a b)))

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
 (make-test for string-to-lexems across test-cases
   '(("1" => ("1"))
     ("1 2 3" => ("1" " " "2" " " "3"))
     ("1,2,3" => ("1" "," "2" "," "3"))
     ("abc" => ("abc"))
     ("a,b,-23" => ("a" "," "b" "," "-23"))
     ("a-2" => ("a-2"))
     ("  " => (" " " "))
     ("ab(cd(ef)12,22" => ("ab" "(" "cd" "(" "ef" ")" "12" "," "22")))))

(define-condition calculator-parse-error (error)
                  ((expression :initarg :expression
                                 :reader calculator-parse-error-expression))
  (:report (lambda (condition stream)
             (format stream "Bad expression ~A"
                     (calculator-parse-error-expression condition)))))

(defun parse (input-string)
  "Parse first expression in input-string using grammar rules.
   Return multiple value list:
    - a first parsed expression as a tree
    - the rest of the lexems as a string.
   String of the rest of the lexems is concatenated from the lexems list
   so it may not be equal to the tail of the input-string."
  (let* ((lexems (string-to-lexems input-string))
	 (lexems (remove-if #'skipped-lexem-p lexems)))
    (multiple-value-bind (p l) (parse-lexems lexems)
      (let ((rest-lexems-string
	     (if l
		 (reduce #'(lambda (a b) (concatenate 'string a " " b)) l)
		 "")))
	(unless p
	  (error 'calculator-parse-error :expression input-string))
	(values p rest-lexems-string)))))

(defun parse-lexems (lexems)
  (parse-with-rule 'exp lexems))

(defun parse-with-rule (rule-name lexems)
  "Parse first rule in lexems list according to the rule with rule-name.
   Return multiple value list:
    - a first parsed expression as a tree
    - the rest of the lexems as a list."
  (declare (type list lexems))
  (etypecase rule-name
    (string (when (string= rule-name (first lexems))
	      (values (list (first lexems))
		      (rest lexems))))
    (list (multiple-value-bind (p l) (parse-with-list-rule rule-name lexems)
	    (when p
	      (values (list p) l))))
    (symbol (let ((rv (get-rule-value (get-rule rule-name)))
		  (cv (get rule-name 'check-value))
		  (producer (or (get rule-name 'product) #'identity))
		  (lexem (first lexems)))
	      (if (null rv)
		  (if cv
		      (let ((cv-result (funcall cv lexem)))
			(when cv-result
			  (values (list (funcall producer lexem))
				  (rest lexems))))
		      (error "No check-value function for ~A rule-name"
			     rule-name))
		  (loop for v in rv
		     do (multiple-value-bind (p l) (parse-with-rule v lexems)
			  (when p
			    (return (values (funcall producer p) l))))))))))

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

;; Test for parse without 'func and 'let producers.
;; (identity function will be used in this case)
(progn
  (setf (get 'func 'product) nil)
  (setf (get 'let  'product) nil)
  (funcall
   (make-test for parse across test-cases
     `(("1" => (1))
       ("3" => (3))
       ("abc" => (,(intern "abc" :interview.calculator.vars)))
       ("add(1, 2)" => ((,(intern 'add-func :interview.calculator.funcs)
			   "(" 1 "," 2 ")")))
       ("div( s ,  add(2,1))"
	=> ((,(intern 'div-func :interview.calculator.funcs) "("
	      ,(intern "s" :interview.calculator.vars) ","
	      (,(intern 'add-func :interview.calculator.funcs)
		"(" 2 "," 1 ")")
	      ")")))))))

;; Adding LISP form producers to the 'func and 'let rules

(defun set-rule-product-function (rule-name product-function)
  "Set PRODUCT-FUNCTION for the RULE-NAME rule."
  (declare (type symbol rule-name))
  (setf (get rule-name 'product) product-function))

(defun produce-func-product (form)
  "Return a LISP form which is a function call form."
  (destructuring-bind ((name |(| exp-1 |,| exp-2 |)|)) form
    (declare (ignore |(| |,| |)|))
    (list (list name exp-1 exp-2))))

(funcall
 (make-test for produce-func-product across test-cases
   '((((add "(" 12 "," 100 ")")) => ((add 12 100))))))

(set-rule-product-function 'func #'produce-func-product)

(defun produce-let-product (form)
  "Return a LISP form which is a ``let'' operator form."
  (destructuring-bind ((name |(| var |,| exp-1 |,| exp-2 |)|)) form
    (declare (ignore name |(| |,| |,| |)|))
    (list (list 'let (list (list var exp-1))
		exp-2))))

(funcall
 (make-test for produce-let-product across test-cases
   '((((let "(" a "," 5 "," a ")")) => ((let ((a 5)) a))))))

(set-rule-product-function 'let #'produce-let-product)

;; Test for parse with LISP-form producers
(funcall
 (make-test for parse across test-cases
   `(("1" => (1))
     ("3" => (3))
     ("abc" => (,(intern "abc" :interview.calculator.vars)))
     ("add(1, 2)" => ((,(intern 'add-func :interview.calculator.funcs) 1 2)))
     ("div( s ,  add(2,1))"
      => ((,(intern 'div-func :interview.calculator.funcs)
	    ,(intern "s" :interview.calculator.vars)
	    (,(intern 'add-func :interview.calculator.funcs) 2 1)))))))

(defun calculator-parse-eval (input-string)
  "Evaluate an expression in the input-string.
   Input is parsing according to the grammar
   and then evaluated by the LISP evaluator.
   Return multiple value list:
    - expression evaluation result
    - string of the rest of the lexems found in the input-string."
  (declare (type string input-string))
  (multiple-value-bind (p l) (parse input-string)
    (when p
      (values (eval (first p)) l))))

(funcall
 (make-test for calculator-parse-eval across test-cases
   '(("add(1, 2)" => 3)
     ("add(1, mult(2, 3))" => 7)
     ("mult(add(2, 2), div(9, 3))" => 12)
     ("let(a, 5, add(a, a))" => 10)
     ("let(a, 5, let(b, mult(a, 10), add(b, a)))" => 55)
     ("let(a, let(b, 10, add(b, b)), let(b, 20, add(a, b)))" => 40))))

;;; User interface

(defun calculator (input-string)
  "Calculate an expression in the input-string.
   Return the result number or error message.
   Input is parsing according to the grammar
   and then evaluated by the LISP evaluator.
   Only one expression is allowed to be in the input-string."
  (if (string= "" input-string)
      (format nil "UI error: input string is empty!")
      (handler-case
	  (multiple-value-bind (result rest)
	      (calculator-parse-eval input-string)
	    (cond ((and result (string= "" rest)) result)
		  ((and result (not (string= "" rest)))
		   (format nil "UI error: input string contains lexems after expression: ~A!"
			   rest))
		  (t
		   (format nil "UI error: unknown error!"))))
	(unbound-variable (v)
	  (format nil "Evaluating error: variable not bound to value: ~A!"
		  (cell-error-name v)))
	(calculator-parse-error (e)
	  (format nil "Parsing error: ~A!" e)))))

(funcall
 (make-test for calculator across test-cases
   '(("add(1, 2)" => 3)
     ("add(1, mult(2, 3))" => 7)
     ("mult(add(2, 2), div(9, 3))" => 12)
     ("let(a, 5, add(a, a))" => 10)
     ("let(a, 5, let(b, mult(a, 10), add(b, a)))" => 55)
     ("let(a, let(b, 10, add(b, b)), let(b, 20, add(a, b)))" => 40)
     ;; errors
     ("" => "UI error: input string is empty!")
     ("1 2"  => "UI error: input string contains lexems after expression: 2!")
     ("1111" => "Parsing error: Bad expression 1111!")
     ("a"    => "Evaluating error: variable not bound to value: a!")
     ("foobar" => "Evaluating error: variable not bound to value: foobar!")
     ("let(a, 1, add(a,b))"    => "Evaluating error: variable not bound to value: b!")
     ("add(x,y)"    => "Evaluating error: variable not bound to value: x!"))))
