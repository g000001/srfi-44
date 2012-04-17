(cl:in-package :srfi-44.internal)

(let ((eof (list nil)))
  (defun eof ()
    eof))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn
    (setf (fdefinition 'eq?) #'cl:eq)
    (setf (fdefinition 'integer?) #'cl:integerp)
    (setf (fdefinition 'negative?) #'cl:minusp)
    (setf (fdefinition 'null?) #'cl:null)
    (setf (fdefinition 'pair?) #'cl:consp)
    (setf (fdefinition 'positive?) #'cl:plusp)
    (setf (fdefinition 'zero?) #'cl:zerop)
    (setf (fdefinition 'vector-length) #'cl:length)
    (setf (fdefinition 'vector?) #'cl:vectorp)
    (setf (fdefinition 'procedure?) #'cl:functionp)
    (setf (fdefinition 'even?) #'cl:evenp)
    (setf (fdefinition 'real?) #'cl:realp)
    (setf (fdefinition 'newline) #'cl:terpri)
    (setf (fdefinition 'display) #'cl:princ)
    (setf (fdefinition 'remainder)  #'cl:rem)
    (setf (fdefinition 'string-length)  #'cl:length)
    (setf (fdefinition 'char->integer)  #'cl:char-code)
    (setf (fdefinition 'string-ref) #'cl:char)
    (setf (fdefinition 'symbol->string) #'cl:string)
    (setf (fdefinition 'string?) #'cl:stringp)
    (setf (fdefinition 'symbol?) #'cl:symbolp)
    (setf (fdefinition 'number?) #'cl:numberp)
    (setf (fdefinition 'char?) #'cl:characterp)
    (setf (fdefinition 'real-part) #'cl:realpart)
    (setf (fdefinition 'imag-part) #'cl:imagpart)
    (setf (fdefinition 'string=?) #'cl:string=)
    (setf (fdefinition 'string-ci=?) #'cl:string-equal)
    (setf (fdefinition 'map) #'cl:mapcar)
    (setf (fdefinition 'char=?) #'cl:char=)
    (setf (fdefinition 'char<?) #'cl:char<)
    (setf (fdefinition 'char-ci=?) #'cl:char-equal)
    (setf (fdefinition 'char-ci<?) #'cl:char-lessp)
    (setf (fdefinition 'string<?) #'cl:string<)
    (setf (fdefinition 'string-ci<?) #'cl:string-lessp)
    (setf (fdefinition 'real?) #'cl:realp)
    (setf (fdefinition 'rational?) #'cl:realp)
    (setf (fdefinition 'exists) #'cl:some)
    ))

(defun make-list (len)
  (cl:make-list len))

(defun complex? (n)
  (numberp n))

(defun exact->inexact (n)
  (float n 0d0))

(defun exact? (n)
  (rationalp n))

(defun inexact? (n)
  (floatp n))

(defun list? (obj)
  (and (cl:listp obj)
       (cl:tailp '() obj)))

(defmacro set! (var val)
  `(setq ,var ,val))

(declaim (cl:inline list-tail vector-set! list-ref vector->list list->vector
                    quotient set-car! set-cdr! eqv?
                    assq assv assoc for-each memq))

(defun eqv? (x y)
  (cl:eql x y))

(defun member (item list)
  (cl:do ((e list (cdr e)))
       ((cl:atom e))
    (cl:when (cl:eql item (car e))
      (cl:return e))))

(defun memq (item list)
  (cl:do ((e list (cdr e)))
       ((cl:atom e))
    (cl:when (cl:eq item (car e))
      (cl:return e))))


(defun for-each (fn cl:&rest lists)
  (cl:apply #'cl:mapc fn lists)
  nil)

(defun assq (item alist)
  (cl:assoc item alist :test #'eq?))

(defun assv (item alist)
  (cl:assoc item alist :test #'eqv?))

(defun assoc (item alist)
  (cl:assoc item alist :test #'equal?))

(defun equal? (x y)
  (cl:equal x y))

(defun set-car! (list obj)
  (cl:rplaca list obj))

(defun set-cdr! (cons x)
  (cl:rplacd cons x))

(defun quotient (x y)
  (values (cl:truncate x y)))

(defun list-tail (list k)
  (cl:nthcdr k list))

(defun list-ref (list k)
  (cl:nth k list))

(defun vector-set! (vec index val)
  (setf (cl:aref vec index) val))

(defun string-set! (str index val)
  (setf (cl:char str index) val))

(defun vector->list (vec)
  (cl:coerce vec 'list))

(defun list->vector (list)
  (cl:coerce list 'cl:vector))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun to-proper-lambda-list (list)
    (cl:typecase list
      (cl:list (if (cl:tailp () list)
                   list
                   (cl:let ((last (cl:last list)))
                     `(,@(cl:butlast list)
                         ,(car last)
                         cl:&rest
                         ,(cdr last)))))
      (cl:symbol `(cl:&rest ,list)))))

(defmacro lambda (args &rest body)
  `(cl:lambda ,(to-proper-lambda-list args)
     ,@body))

(defmacro letrec ((&rest binds) &body body)
  `(let (,@(cl:mapcar (cl:lambda (x)
                        `(,(car x) #'values) )
             binds ))
     (declare (optimize (space 3) (debug 1)))
     (labels (,@(cl:remove nil
                  (cl:mapcar (cl:lambda (x &aux (name (car x)))
                               `(,name
                                 (&rest args)
                                 (apply ,name args) ))
                             binds )))
       (declare (optimize (debug 1) (space 3)))
       (psetq ,@(cl:apply #'cl:append binds))
       ,@body )))

(defmacro define-function (name-args &body body)
  (if (cl:consp name-args)
      (cl:destructuring-bind (name . args)
                             name-args
        `(defun ,name ,(to-proper-lambda-list args)
           ,@body))
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (setf (fdefinition ',name-args)
                 (function cl:values) ))
         (setf (fdefinition ',name-args)
               ,(car body) ))))

(declaim (inline vector-ref))
(defun vector-ref (vec k)
  (cl:svref vec k))

(declaim (inline modulo))
(defun modulo (x y)
  (cl:mod x y))

(defmacro begin (&body body)
  `(progn ,@body))

(declaim (inline make-vector))
(defun make-vector (size &optional (init 0))
  (cl:make-array size                   ;***
                 :initial-element init
                 :adjustable nil
                 :fill-pointer nil))

(declaim (inline string-append))
(defun string-append (&rest strings)
  (cl:format nil "~{~A~}" strings))

(declaim (inline number->string))
(defun number->string (num)
  (cl:write-to-string num))

(defmacro dolex ((&rest varlist) endlist &body body)
  (let* ((vars (cl:mapcar (lambda (v)
                            (if (cl:consp v) (car v) v) )
                          varlist ))
         (binds (cl:mapcar (lambda (b)
                             (if (cl:consp b)
                                 (cl:destructuring-bind (var &optional init next)
                                      b
                                   (if next
                                       `(,var ,init
                                              (let (,@(cl:mapcar (lambda (x)
                                                                   (list x x) )
                                                        vars ))
                                                (declare (ignorable ,@vars))
                                                ,next ))
                                       `(,var ,init) ))
                                 (list b nil) ))
                           varlist )))
    `(cl:do ,binds ,endlist ,@body) ))


(defmacro with-local-define-function (&body defines-body)
  (or (cl:member :in defines-body) (error "no body"))
  (let* ((body-pos (cl:position :in defines-body))
         (defines  (cl:subseq defines-body 0 body-pos))
         (body     (cl:subseq defines-body (cl:1+ body-pos))) )
    (cl:loop
       :for (nil name-arg . bo) :in defines
       :collect (cl:let ((name-arg (to-proper-lambda-list name-arg)))
                  `(,(car name-arg) ,(cdr name-arg) ,@bo) )
       :into defs
       :finally (cl:return
                  `(labels (,@defs)
                     ,@body )))))

(defmacro with-local-define-variable (&body defines-body)
  (or (cl:member :in defines-body) (error "no body"))
  (let* ((body-pos (cl:position :in defines-body))
         (defines  (cl:subseq defines-body 0 body-pos))
         (body     (cl:subseq defines-body (cl:1+ body-pos))) )
    (cl:loop
       :for (nil v bo) :in defines
       :collect v :into vars
       :collect v :into setqs
       :collect bo :into setqs
       :finally (cl:return
                  `(cl:let (,@vars)
                     (cl:psetq ,@setqs)
                     ,@body )))))

(defun boolean? (obj)
  (cl:typep obj '(cl:member cl:t cl:nil)))


(defun eof-object? (obj)
  (eq obj (eof)))

(defmacro iterate (tag specs &body body)
  (let ((vars  (mapcar #'car specs))
        (vals  (mapcar #'cadr specs))
	(id    (gensym))
        (dvars (map-into (make-list (length specs)) #'gensym)))
    `(block ,id
       (let ,(mapcar #'list dvars vals)
         (macrolet ((,tag ,vars
                      `(progn (psetq ,@(list ,@(mapcan #'(cl:lambda (dvar var)
                                                           `(',dvar ,var))
                                                       dvars
                                                       vars)))
                              (go ,',id))))
           (tagbody
             ,id
             (let ,(mapcar #'list vars dvars)
               (return-from ,id (progn ,@body)))))))))

(defun dynamic-wind (in body out)
  (declare (function in body out))
  (funcall in)
  (unwind-protect (funcall body)
    (funcall out)))


(defun call-with-values (producer consumer)
  (multiple-value-call consumer (funcall producer)))

(defun input-port? (port)
  (input-stream-p port))

(defun output-port? (port)
  (output-stream-p port))

(defun memv (item list)
  (member item list))

(defun vector-fill! (vec item)
  (declare (type vector vec))
  (fill vec item))

;; ----
;; Copyright (C) 2003 Taylor Campbell and Scott G. Miller.  See the LICENCE
;; file for details.

;; (cl:in-package :srfi-44.internal)

;; This file requires SRFI 23 (error).

(define-syntax let*-optionals
  (syntax-rules ()
    ((_ ?proc (?rest ***) ((?var ?default) ***) ?e1 ?e2 ***)
     (let ((rest (?rest ***)))
       (let*-optionals ?proc rest ((?var ?default) ***) ?e1 ?e2 ***)))
    ((_ ?proc ?rest () ?e1 ?e2 ***)
     (if (null? ?rest)
         (begin ?e1 ?e2 ***)
         (error "Too many arguments" ?proc)))
    ((_ ?proc ?rest ((?var1 ?default1) (?var2 ?default2) ***) ?e1 ?e2 ***)
     (list-case ?rest
       (lambda ()
         (let* ((?var1 ?default1) (?var2 ?default2) ***) ?e1 ?e2 ***))
       (lambda (?var1 new-rest)
         (let*-optionals ?proc new-rest ((?var2 ?default2) ***)
           ?e1 ?e2 ***))))))

(define-function (check-arg pred? arg caller)
  (if (not (funcall pred? arg))
      (error "Bad argument"
             pred?
             arg
             caller)))

(define-function (always . vals) (lambda _
                                   (declare (ignore _))
                                   (apply #'values vals)))

(define-syntax receive
  (syntax-rules ()
    ((_ ?formals ?producer ?body1 ?body2 ***)
     (call-with-values
       (lambda () ?producer)
       (lambda ?formals ?body1 ?body2 ***)))))

(define-syntax rec
  (syntax-rules ()
    ((rec (?f . ?args) ?body1 ?body2 ***)
     (rec ?f (lambda ?args ?body1 ?body2 ***)))
    ((rec ?x ?y)
     (letrec ((?x ?y)) ?x))))

(define-function (drop l k)
  (if (zero? k)
      l
      (drop (cdr l) (- k 1))))

(define-function (take l k)
  (if (zero? k)
      '()
      (cons (car l) (take (cdr l) (- k 1)))))

(define-function (fold-right kons knil l)
  (if (null? l)
      knil
      (funcall kons (car l) (fold-right kons knil (cdr l)))))

(define-function (list-case l k-nil k-pair)
  (cond ((null? l)
         (funcall k-nil))
        ((pair? l)
         ;; Use CAR+CDR from SRFI 1, perhaps?
         ;(call-with-values (lambda () (car+cdr l)) k-pair)
         (funcall k-pair (car l) (cdr l)))
        (:else
         (error "Not a list" l))))

(define-function (maybe-cars+cdrs l sk fk)
  (list-case l
    (lambda () (funcall sk '() '()))
    (lambda (list1 list2+)
      (list-case list1
        fk
        (lambda (car1 cdr1)
          (maybe-cars+cdrs list2+
            (lambda (car2+ cdr2+)
              (funcall sk (cons car1 car2+)
                  (cons cdr1 cdr2+)))
            fk))))))

;;; ----
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
