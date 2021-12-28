;;;; cl-utter.lisp

(in-package :cl-utter)


;; First before anything I'm going to grab my debug loggers


(defparameter *log-level* 'debug) 	;; Debug is the most verbose level, error is least verbose
(defparameter *all-log-levels* '(debug info warning error))
(defparameter *save-logs* t)

(defun log-new-list ()
  (member *log-level* *all-log-levels*))

(defun make-log (level string &rest args)
  (when (member level (log-new-list))
    (with-open-file (str "clutter.txt"
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist  :create) 
      (format str "[~A] " level)
      (apply #'format str string args)
      (format str "~%")
      (format t "[~A] " level)
      (apply #'format t string args)
      (format t "~%"))))

;; It should take a set of tests that return pass or fail regarding on the test
;; It should have a function that initialises the testing of *all* the tests.
;; If any fail it should tell us which ones failed and tell us the problem
;; We want to preserve property from the list of tests for the user to do their own further analyses/debugging

;; So let's define some tests

;; We can make a macro that logs and tests
;; (assertion eq (+ 1 2) 4)

(defmacro assertion (comparator function expected)
  `(progn
     (make-log 'debug "testing whether ~A is ~A ~A" ',function ',comparator ',expected)
     (handler-case 
         (if (,comparator ,function ,expected)
	     (progn (make-log 'info "Pass")
                    t)
	     (progn (make-log 'error "Expected ~A ~A, but got ~A for ~A" ',comparator ,expected ,function ',function)
		    (list 'fail ',comparator ',function ',expected)))
       (error ()
         (progn
           (make-log 'debug "Assertion threw an error, checking all components of the test are valid")
           (mapc (lambda (component)
                   (handler-case (eval component)
                     (error ()
                       (make-log 'error "The expression ~A cannot be evaluated" component))))
                 (list ',function ',expected (list ',comparator '*function* '*expected*)))
           (list 'fail ',comparator ',function ',expected)
           )))))


(defparameter *all-tests* nil)
(defparameter *function* 1)
(defparameter *expected* 1)


(defmacro deftest (sym &body body)
  `(progn
     (defun ,sym ()
       ,@body)
     (pushnew ',sym *all-tests*)))


(defmacro with-mock (sym lam &body body)
  "Mocking functions with other functions! Utilising lexical scoping"
  `(labels ((,sym ,(car lam) ,(second lam)))
     ,@body))

;; Example tests

;; (deftest example-test-2
;;   (assertion member 3 5))

;; (deftest example-test-3
;;   (let ((examplet (/ / 15)))
;;     (assertion < examplet 5)))
;; ERROR!!!!!


;; Test group
(defun test-group (group)
  "You can define your own groups too. Just make a parameter. Soon I'll let you push automatically when you deftest"
  (make-log 'debug "testing the following: ~A" (reverse group))
  (mapcar (lambda (test)
            (make-log 'info "~A" test)
            (funcall test))
          (reverse group)))

;; Test all
(defun test-all ()
  (make-log 'debug "testing the following: ~A" (reverse *all-tests*))
  (mapcar (lambda (test)
            (make-log 'info "~A" test)
            (funcall test)) (reverse *all-tests*)))


;; Now you can just change *all-tests* to your tests and (test-all) will work for you!

;; Things to add next include throwing the user into a debugger
;; Test groups for testing only certain ones
