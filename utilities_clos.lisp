;; Copyright (C) 2003 Taylor Campbell and Scott G. Miller.  See the LICENCE
;; file for details.

;; This file requires Tiny-CLOS and utilities.scm.

(cl:in-package :srfi-44.internal)

(define-function (subclass? c1 c2)
  (or (eq? c2 <top>)
      (and (memq c2 (class-cpl c1)) 'T)))

(define-function (make-generic-predicate class)
  (let ((g (make-generic)))
    (add-method g
      (make-method (list <object>)
        (constantly nil)))
    (add-method g
      (make-method (list class)
        (constantly 'T)))
    g))

(define-function (make-predicate-method class)
  (declare (ignore class))
  (make-method (list <class>)
    (constantly 'T)))

(define-function (make-generic/constraint . classes)
  (let ((g (make-generic)))
    (add-method g
      (make-method (list <top>)
        (lambda (call-next-method . args)
          (declare (ignore call-next-method args))
          (apply #'error "Can't apply to anything but with arguments of"
                 classes))))
    g))

(define-syntax define-constrained-generics
  (syntax-rules ()
    ((define-constrained-generics ?class ?generic)
     (cl:defgeneric ?generic ( ?class)))
    ((define-constrained-generics ?class ?generic ?more ***)
     (begin
       (define-constrained-generics ?class ?generic)
       (define-constrained-generics ?class ?more ***)))))

(define-syntax define-supertype-handled
  (syntax-rules ()
    ((define-supertype-handled ?pred?
       ((?f ?arg1 . ?args) ?super)
       ***)
     (begin
       (define-function (?f ?arg1 . ?args)
         (check-arg #'?pred? ?arg1 '?f)
         (generate-call ?super (?arg1 . ?args) ()))
       ***))))

(define-syntax generate-call
  (syntax-rules ()
    ((generate-call ?super () ?args)
     (?super . ?args))
    ((generate-call ?super (?arg . ?more) (?done ***))
     (generate-call ?super ?more (?done *** ?arg)))
    ((generate-call ?super ?last (?arg ***))
     (apply #'?super ?arg *** ?last))))

(define-function (class-predicate class)
  (lambda (x) (subclass? (class-of x) class)))

(define-function (function->method specs f)
  (make-method specs
    (lambda (call-next-method . args)
      (declare (ignore call-next-method))
      (apply f args))))

;; Convenience macro for making methods.
(define-syntax method
  (syntax-rules ()
    ((_ ?next-method (?param . ?more-lambda-list) ?e1 ?e2 ***)
     (method-aux () () 'NIL (?param . ?more-lambda-list)
                 ?next-method
                 ?e1 ?e2 ***))))

(define-syntax method-aux
  (syntax-rules ()
    ((_ (?spec ***) (?param ***) 'NIL () ?next-method ?e1 ?e2 ***)
     (make-method (list ?spec ***)
       (lambda (?next-method ?param ***)
         (declare (ignorable ?next-method ?param ***))
         ?e1 ?e2 ***)))
    ((_ (?spec ***) (?param ***) ?rest () ?next-method ?e1 ?e2 ***)
     (make-method (list ?spec ***)
       (lambda (?next-method ?param *** . ?rest)
         ?e1 ?e2 ***)))

    ((_ (?spec ***) (?param ***) 'NIL ((?p ?class) . ?more)
        ?next-method
        ?e1 ?e2 ***)
     (method-aux (?spec *** ?class) (?param *** ?p) 'NIL ?more
                 ?next-method
                 ?e1 ?e2 ***))
    ((_ (?spec ***) (?param ***) 'NIL (?p . ?more)
        ?next-method
        ?e1 ?e2 ***)
     (method-aux (?spec *** <top>) (?param *** ?p) 'NIL ?more
                 ?next-method
                 ?e1 ?e2 ***))

    ((_ (?spec ***) (?param ***) 'NIL ?rest ?next-method ?e1 ?e2 ***)
     (method-aux (?spec ***) (?param ***) ?rest ()
                 ?next-method
                 ?e1 ?e2 ***))))

(define-function (unzip3 lists)
  (if (null? lists)
      (values '() '() '())
      (receive (z1 z2 z3) (unzip3 (cdr lists))
        (values (cons (caar   lists) z1)
                (cons (cadar  lists) z2)
                (cons (caddar lists) z3)))))

(define-function (add-methods . generic+function+specs-lists)
  (receive (generics functions specializers-lists)
      (unzip3 generic+function+specs-lists)
    (for-each (lambda (generic function specializers)
;                 (breakpoint
;                  "Adding method ~S to generic ~S with specs ~S"
;                  function generic specializers)
                (add-method generic
                  (function->method specializers function)))
              generics functions specializers-lists)))


#|(define-function class-of
  (lambda (x)
    (or (cond ((tiny-clos.internal::%instance? x)
               (tiny-clos.internal::%instance-class  x) )
              ((pair? x)        <pair>)         ;If all Schemes were IEEE
              ((null? x)        <null>)         ;compliant, the order of
              ((boolean? x)     <boolean>)      ;these wouldn't matter?
              ((symbol? x)      <symbol>)
              ((procedure? x)   <procedure>)
              ((number? x)      <number>)
              ((vector? x)      <vector>)
              ((char? x)        <char>)
              ((string? x)      <string>)
              ((input-port? x)  <input-port>)
              ((output-port? x) <output-port>)
              (T nil) )
        (tiny-clos:class-of x))))|#

;; (defparameter <null> (make-primitive-class))
;; (defvar <pair>        (make-primitive-class))
;; (defvar <symbol>      (make-primitive-class))
;; (defvar <boolean>     (make-primitive-class))
;; (defvar <procedure>   (make-primitive-class <procedure-class>))
;; (defvar <number>      (make-primitive-class))
;; (defvar <vector>      (make-primitive-class))
;; (defvar <char>        (make-primitive-class))
;; (defvar <string>      (make-primitive-class))
;; (defparameter <input-port> (make-primitive-class))
;; (defparameter <output-port> (make-primitive-class))
