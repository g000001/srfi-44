(cl:in-package :srfi-44.internal)

#||
(defvar *string*
  "foo")


(collection?  *string*)
;=>  "foo"

(collection= *string* *string*)
;=>  T

(collection-copy *string*)
;=>  "foo"

(collection-size *string*)
;=>  "foo"

(collection->list "foo")
;=>  "foo"

(collection?  '())
;=>  NIL

(collection= () ())


(eq <list> (class-of ()))
;=>  T

(collection-size "foo")

(collection-size '(1 2 3 4))

(list? ())
||#

(make-list 3)
;=>  (:UNDEF :UNDEF :UNDEF)


(list-fold-keys-left '(1 2 3 4)
                     (lambda (x y)
                       (print (list x y))
                       (+ x y))
                     )

(list-case '(8)
           (lambda () nil)
           (lambda (x y) (list x y)))
;=>  (8 NIL)


(receive (proceed? . new-knils)
         (apply (lambda (x y) (+ x y)) 0 1 NIL)
  (list proceed? new-knils))

(list-fold-left '(1 2 3 4)
                (lambda (x)
                  (print x)))

(define-function  (always-proceed f)
  (lambda knils
    (receive new-knils (apply f knils)
      (apply #'values t new-knils))))


(list-fold-left (list 1 2 3)
                (always-proceed #'cons)
                nil)
;=>  (3 2 1)

(vector-fold-left #(1 2 3)
                  (always-proceed #'+)
                  0)

(vector-size #(1 2 3) )

(vector-ref #(1 2 3) 0)
(string-ref "123" 8)

(string-count "1234" #\1)
(vector-count #(1 2 3 4) 1)

(receive (a . b) (values 1 2)
  (list a b))



'((a . 1) (b . 2))
(alist-map-size (make-alist-map) )

(let ((am (make-alist-map)))
  (alist-map-put! am :a 1)
  (alist-map-put! am :b 2)
  #|(slot-value am 'contents)|#
  ;;
  (alist-map-get (alist-map-put am :b 2) :b)
  (alist-map= #'equal
              (alist-map-put am :b 2)
              (alist-map-put am :b 2))
  )
#|(bag= #'eql
      (make-instance <bag> :contents '(1 2 2 2 2 2 3))
      (make-instance <bag> :contents '(1 2 3 3)))|#
