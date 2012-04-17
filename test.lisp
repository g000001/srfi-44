(cl:in-package :srfi-44.internal)

;
(test :Construction
  (is-true (progn (defparameter ls1 (list 1 2 3)) t))
  (is-true (progn (defparameter ls2 (make-list)) t))
  (is-true (progn (defparameter vec1 (vector 1 2 3)) t))
  (is-true (progn (defparameter vec2 (make-vector 3)) t))
  (is-true (progn (defparameter str1 (string #\a #\b #\c)) t))
  (is-true (progn (defparameter str2 (make-string 3 #\space)) t))
  (is-true (progn (defparameter amap1
                    (alist-map #'equal? '(a . 1) '(b . 2) '(c . 3))) t))
  (is-true (progn (defparameter amap2 (make-alist-map #'equal?)) t)))

(defparameter idx1 '((0 . 1) (1 . 2) (2 . 3)))
(defparameter ridx1 '((2 . 3) (1 . 2) (0 . 1)))
(defparameter idx2 '((0 . #\a) (1 . #\b) (2 . #\c)))
(defparameter ridx2 '((2 . #\c) (1 . #\b) (0 . #\a)))

(define-function (always-proceed f)
  (lambda knils
    (receive new-knils (apply f knils) (apply #'values
                                              'T new-knils))))

(define-function apsap (always-proceed
                        (lambda (c seed)
                          (string-append (string c) seed))))

(define-function ap+ (always-proceed #'+))
(define-function ap- (always-proceed #'-))
(define-function apcons2 (always-proceed (lambda (a b c) (cons (cons a b) c))))

(define-syntax test*
  (syntax-rules (=>)
    ((test* name
            (num expr => result) ***)
     (test name
       (is (equal result expr)) ***))))

(test* :Enumeration
  (0  (collection-fold-left ls1 #'ap+ 0) => 6)
  (1  (list-fold-left ls1 #'ap+ 0) => 6)
  (2  (collection-fold-right ls1 #'ap- 0) => 2) ;
  (3  (list-fold-right ls1 #'ap- 0) => 2) ;
  (4  (collection-fold-keys-left ls1 #'apcons2 '()) => ridx1)
  (5  (list-fold-keys-left ls1 #'apcons2 '()) => ridx1)
  (6  (collection-fold-keys-right ls1 #'apcons2 '()) => idx1) ;
  (7  (list-fold-keys-right ls1 #'apcons2 '()) => idx1) ;
  (8  (collection-fold-left vec1 #'ap+ 0) => 6)
  (9  (vector-fold-left vec1 #'ap+ 0) => 6)
  (10  (collection-fold-right vec1 #'ap- 0) => 2) ;
  (11  (vector-fold-right vec1 #'ap- 0) => 2)
  (12  (collection-fold-keys-left vec1 #'apcons2 '()) => ridx1)
  (13  (vector-fold-keys-left vec1 #'apcons2 '()) => ridx1)
  (14  (collection-fold-keys-right vec1 #'apcons2 '()) => idx1)
  (15  (vector-fold-keys-right vec1 #'apcons2 '()) => idx1)
  (16  (collection-fold-left str1 #'apsap "d") => "cbad")
  (18  (collection-fold-right str1 #'apsap "d") => "abcd") ;error
  (20  (collection-fold-left amap1 #'ap+ 0) => 6)
  (21  (alist-map-fold-left amap1 #'ap+ 0) => 6)
  (22  (collection-fold-right amap1 #'ap+ 0) => 6) ;
  (23  (alist-map-fold-right amap1 #'ap+ 0) => 6) ;
  (24  (collection-fold-keys-left str1 #'apcons2 '()) => ridx2)
  (25  (string-fold-keys-left str1 #'apcons2 '()) => ridx2)
  (26  (collection-fold-keys-right str1 #'apcons2 '()) => idx2)
  (27  (string-fold-keys-right str1 #'apcons2 '()) => idx2)
  )

#||||
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

(sequence= #'=
           '(1 2 3 4)
           '(1 2 3 4))

(collection-name "foo")

(vector= #'eql #() #())
||||#
