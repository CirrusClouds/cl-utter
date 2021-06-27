;;;; cl-unittest.lisp

(in-package :cl-unittest)


;; First before anything I'm going to grab my debug loggers


(defparameter *log-level* 'debug) 	;; Debug is the most verbose level, error is least verbose
(defparameter *all-log-levels* '(debug info warning error))

(defun log-new-list ()
  (member *log-level* *all-log-levels*))

(defun make-log (level string &rest args)
  (when (member level (log-new-list))
    (apply #'format t string args)
    (terpri)))


;; Now let's talk about design specs. What do we want from a unit testing framework?
;; It should take a set of tests that return true or false regarding on the test
;; It should have a function that initialises the testing of *all* the tests.
;; If any fail it should tell us which ones failed and tell us the problem

;; So let's define some tests

;; We can make a macro that logs and tests


(defmacro assertion (comparator function expected)
  `(progn
     (make-log 'debug "testing whether ~A is ~A ~A" ',function ',comparator ',expected)
     (handler-case 
         (if (,comparator ,function ,expected)
	     (progn (make-log 'info "Pass")
                    t)
	     (progn (make-log 'error "Expected ~A ~A, but got ~A for ~A" ',comparator ,expected ,function ',function)
		    (list 'fail ',comparator ',function ',expected)))
       (error (c)
         (progn
           (make-log 'debug "Assertion threw an error, checking all components of the test are valid")
           (mapc (lambda (component)
                     (handler-case (eval component)
                       (error (c1)
                         (make-log 'error "The expression ~A cannot be evaluated" component))))
                   (list (list ',comparator '*function* '*expected*) ',function ',expected))
           (list 'fail ',comparator ',function ',expected)
           )))))


(defparameter *all-tests* nil)
(defparameter *function* 1)
(defparameter *expected* 1)


(defmacro deftest (sym comparator function expected)
  `(progn
     (defun ,sym ()
       (assertion ,comparator ,function ,expected))
     (pushnew ',sym *all-tests*)))

;; Example tests

(deftest example-test-1 eq (+ 1 2) 3)
(deftest example-test-2 eq (+ 1 2) 4)
(deftest example-test-3 < (+ 1 5) 4)
(deftest example-test-4 < (/ 1 5) (- 1 4))
(deftest example-test-5 < (/ / 15) 5)
(deftest example-test-6 x (/ 1 5) (- 1 4))


;; Test all
(defun test-all ()
  (make-log 'debug "testing the following: ~A" (reverse *all-tests*))
  (mapcar (lambda (test)
            (make-log 'info "~A" test)
            (funcall test)) (reverse *all-tests*)))


;; Now you can just change *all-tests* to your tests and (test-all) will work for you!

;; Things to add next include throwing the user into a debugger
;; Test groups for testing only certain ones
