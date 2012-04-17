;; Copyright (C) 2003 Taylor Campbell and Scott G. Miller.  See the
;; LICENCE file for details.

;; This reference implementation uses Gregor Kiczales' Tiny-CLOS.

;; This file requires utilities.scm, utilities_tiny-clos.scm, SRFI 2
;; (and-let*), SRFI 23 (error), and Tiny-CLOS.

(cl:in-package :srfi-44.internal)

;; It hacks up the class hierarchy in Tiny-CLOS.

;; The abstract collection type hierarchy
(defvar <collection> (make-class (list <object>) '()))

(defvar <bag>               (make-class (list <collection>) '()))
(defvar <sequence>          (make-class (list <bag>)        '()))
(defvar <flexible-sequence> (make-class (list <sequence>)   '()))

(defvar <set>               (make-class (list <collection>) '()))

(defvar <map>               (make-class (list <collection>) '()))

;; Attributes
(defvar <attribute> (make-class (list <top>) '()))
(define-function (make-attribute) (make-class (list <attribute>) '()))
(defvar <ordered-attribute>        (make-attribute))
(defvar <directional-attribute>    (make-attribute))
(defvar <purely-mutable-attribute> (make-attribute))
(defvar <limited-attribute>        (make-attribute))

(define-function collection-get-any #'values) ;dummy

;; The concrete collection types

(defvar <list>  (make-class (list <flexible-sequence> <pair> <null>)
                            '()))

(defvar <alist-map>
  (make-class (list <map>) '(comparator contents)))

(set! <vector>
  (make-class (list <sequence>
                    <purely-mutable-attribute>
                    <limited-attribute>)
              '()))
(set! <string>
  (make-class (list <sequence>
                    <purely-mutable-attribute>
                    <limited-attribute>)
              '()))

;; NOTE: Requires access to the low-level %INSTANCE? & %INSTANCE-CLASS
;; function from Tiny-CLOS.
(define-function class-of
  (lambda (obj)
    (cond
     ((%instance? obj)
      (%instance-class obj))

     ((list-mumble  obj) <list>)
     #|((list-mumble  obj))|#

     ((string?      obj) <string>)
     ((vector?      obj) <vector>)

     ;; Tiny-CLOS needs full numeric tower support.
     ((number?      obj) <number>)

     ((char?        obj) <char>)
     ((symbol?      obj) <symbol>)
     ((procedure?   obj) <procedure>)

     ((input-port?  obj) <input-port>)
     ((output-port? obj) <output-port>)
     (:else <top>) )))

(define-function (list-mumble x)
  (cond ((null? x)
         ;; Should this be <NULL>?
         <list>)
        ((pair? x)
         (iterate loop ((x x))
           (cond ((null? x) <list>)
                 ((pair? x) (loop (cdr x)))
                 (:else      <pair>))))
        (:else 'NIL)))

;;; - Folding -

(define-constrained-generics <collection>
  collection-fold-left
  collection-fold-right
  collection-fold-keys-left
  collection-fold-keys-right)

;;; - General Collections -

(define-function collection? (class-predicate <collection>))

(define-constrained-generics <collection>
  collection-name
  collection-size
  collection-count
  collection-get-any
  collection-empty?
  collection->list
  collection-clear collection-clear!
  collection-copy)

(define-function collection= (make-generic))

(add-method #'collection-size
  (method next-method ((coll <collection>))
    (collection-fold-left coll
      (lambda (x s)
        (declare (ignore x))
        (values 'T (+ s 1)))
      0)))

(add-method #'collection-count
  (method next-method ((coll <collection>) elt)
    (collection-fold-left coll
      (lambda (elt* count)
        (values 'T
                ;; Oh well.
                (if (eqv? elt elt*)
                    (+ count 1)
                    count)))
      0)))

(add-method #'collection-empty?
  (method next-method ((coll <collection>))
    (zero? (collection-size coll))))

(add-method #'collection->list
  (method next-method ((coll <collection>))
    (collection-fold-right coll
      (lambda (x tail)
        (values 'T (cons x tail)))
      '())))


(add-method #'collection=
  (method next-method (elt=?)
    'T))

(add-method #'collection=
  (method next-method (elt=? (coll <collection>))
    'T))
(add-method #'collection=
  (method next-method (elt=? (col1 <sequence>) (col2 <sequence>) . more)
    (declare (ignore next-method))
    (and (= (sequence-size col1) (sequence-size col2))
           (iterate loop ((i (- (sequence-size col1) 1)))
             (cond ((< i 0) 'T)
                   ((funcall elt=? (sequence-ref col1 i) (sequence-ref col2 i))
                    (loop (- i 1)))
                   (:else 'NIL)))
           (apply #'collection= elt=? col2 more))))

(add-method #'collection=
  (make-method (list <top> <map> <map>)
    (lambda (next-method elt=? . args)
      (declare (ignore next-method))
      (apply (lambda (col1 col2 . more)
               (and (= (map-size col1) (map-size col2))
                    (or (map-empty? col1)
                        (collection-fold-keys-left col1
                          (lambda (k v s)
                            (declare (ignore s))
                            (if (and (map-contains-key? col2 k)
                                     (funcall elt=? v (map-get col2 k)))
                                (values 'T 'T)
                                (values 'NIL 'NIL)))
                          'T))
                    (apply #'collection= elt=? col2 more)))
             args))))

;;; - Attributes -

(define-function ordered-collection? (class-predicate <ordered-attribute>))

(define-constrained-generics <ordered-attribute>
  collection-ordering-function

  collection-get-left
  collection-get-right

  collection-delete-left  collection-delete-left!
  collection-delete-right collection-delete-right!)

(define-function directional-collection?
  (class-predicate <directional-attribute>))

(define-constrained-generics <directional-attribute>
  collection-insert-left  collection-insert-left!
  collection-insert-right collection-insert-right!)

(define-function purely-mutable-collection?
  (class-predicate <purely-mutable-attribute>))

(define-function limited-collection? (class-predicate <limited-attribute>))

;;; - Sets -

(define-function set? (class-predicate <set>))

(define-supertype-handled set?
  ((set-size set)
   collection-size)
  ((set-count set value)
   collection-count)
  ((set-get-any set . maybe-fk)
   collection-get-any)
  ((set-empty? set)
   collection-empty?)
  ((set->list set)
   collection->list)
  ((set-clear set)
   collection-clear)
  ((set-clear! set)
   collection-clear!)
  ((set-copy set)
   collection-copy))

(define-function (set= elt=? . sets)
  (for-each (lambda (set) (check-arg #'set? set 'set=)) sets)
  (apply #'collection= elt=? sets))

(define-constrained-generics <set>
  set-contains?
  set-comparator
  set-contains?
  set-subset?
  set-add set-add!
  set-delete set-delete!
  set-union set-union!
  set-intersection set-intersection!
  set-difference set-difference!
  set-symmetric-difference set-symmetric-difference!
  set-add-from set-add-from!
  set-delete-from set-delete-from!)

(add-method #'collection-count
  (method next-method ((set <set>) elt)
    (if (set-contains? set elt) 1 0)))

(add-method #'collection=
  (method next-method ((elt=? <top>) (set1 <set>) (set2 <set>) . more)
          (declare (ignore next-method))
          (display "e")

    (and (collection-fold-left set1
           (lambda (elt _)
             (declare (ignore _))
             (if (set-contains? set2 elt)
                 (values 'NIL 'T)
                 (values 'T 'NIL)))
           'NIL)
         (apply #'collection= elt=? set2 more))))

;;; - Bags -

(define-function bag? (class-predicate <bag>))

(define-supertype-handled bag?
  ((bag-size bag)
   collection-size)
  ((bag-count bag value)
   collection-count)
  ((bag-get-any bag . maybe-fk)
   collection-get-any)
  ((bag-empty? bag)
   collection-empty?)
  ((bag->list bag)
   collection->list)
  ((bag-clear bag)
   collection-clear)
  ((bag-clear! bag)
   collection-clear!)
  ((bag-copy bag)
   collection-copy))

(define-constrained-generics <bag>
  bag-comparator
  bag-contains?
  bag-add bag-add!
  bag-delete bag-delete!
  bag-delete-all bag-delete-all!
  bag-add-from bag-add-from!
  bag-delete-from bag-delete-from!
  bag-delete-all-from bag-delete-all-from!)

(add-method #'bag-contains?
  (method next-method ((bag <bag>) value)
    (let ((elt=? (bag-comparator bag)))
      (collection-fold-left bag
        (lambda (elt _)
          (declare (ignore _))
          (if (funcall elt=? elt value)
              (values 'NIL 'T)
              (values 'T 'NIL)))
        'NIL))))

;;; - Sequences -

(define-function sequence? (class-predicate <sequence>))

(define-supertype-handled sequence?
  ((sequence-size seq)
   collection-size)
  ((sequence-count seq value)
   collection-count)
  ((sequence-get-any seq . maybe-fk)
   collection-get-any)
  ((sequence-empty? seq)
   collection-empty?)
  ((sequence->list seq)
   collection->list)
  ((sequence-clear seq)
   collection-clear)
  ((sequence-clear! seq)
   collection-clear!)

  ((sequence-comparator seq)
   bag-comparator)
  ((sequence-contains? seq value)
   bag-contains?)
  ((sequence-add seq value)
   bag-add)
  ((sequence-add! seq value)
   bag-add!)
  ((sequence-delete seq value)
   bag-delete)
  ((sequence-delete! seq value)
   bag-delete!)
  ((sequence-delete-all seq value)
   bag-delete-all)
  ((sequence-delete-all! seq value)
   bag-delete-all!)
  ((sequence-delete-all-from seq bag)
   bag-delete-all-from)
  ((sequence-delete-all-from! seq bag)
   bag-delete-all-from!)
  ((sequence-add-from seq value)
   bag-add-from)
  ((sequence-add-from! seq value)
   bag-add-from!)
  ((sequence-delete-from seq value)
   bag-delete-from)
  ((sequence-delete-from! seq value)
   bag-delete-from!))

(add-method #'collection-fold-left
  (method next-method ((seq <sequence>) f . seeds)
    (declare (ignore next-method))
    (let ((size (sequence-size seq))
          (seed-count (list-size seeds)))
      (iterate loop ((seeds seeds) (i 0))
        (if (>= i size)
            (apply #'values seeds)
            (receive (proceed? . new-seeds)
                (apply f seeds)
              (if (= (list-size new-seeds) seed-count)
                  (if proceed?
                      (loop new-seeds (+ i 1))
                      (apply #'values new-seeds))
                  (error "(sequence)-fold-left: Wrong seed count"
                         `(expected ,seed-count)
                         `(got ,(list-size new-seeds))))))))))

(add-method #'collection-fold-right
  (method next-method ((seq <sequence>) f . seeds)
    (declare (ignore next-method))
    (let ((size (sequence-size seq))
          (seed-count (list-size seeds)))
      (srfi-5:let loop ((seeds seeds) (i (- size 1)))
        (if (negative? i)
            (apply #'values seeds)
            (receive (proceed? . new-seeds)
                (apply f seeds)
              (if (= (list-size new-seeds) seed-count)
                  (if proceed?
                      (loop new-seeds (- i 1))
                      (apply #'values new-seeds))
                  (error "(sequence)-fold-right: Wrong seed count"
                         `(expected ,seed-count)
                         `(got ,(list-size new-seeds))))))))))

(add-method #'collection-get-any
  (method next-method ((seq <sequence>) . maybe-ft)
    (declare (ignore next-method))
    (if (zero? (sequence-size seq))
        (if (pair? maybe-ft) (funcall (car maybe-ft)) 'NIL)
        (sequence-ref seq 0))))

(define-constrained-generics <sequence>
  sequence-ref
  sequence-get-left
  sequence-get-right
  sequence-set
  sequence-set!
  sequence-replace-from
  sequence-replace-from!
  sequence-insert-right
  sequence-insert-right!
  sequence-copy)

(add-method #'sequence-get-left
  (method next-method ((seq <sequence>) . maybe-fk)
    (declare (ignore next-method))
    (if (collection-empty? seq)
        (and (pair? maybe-fk) (funcall (car maybe-fk)))
        (sequence-ref seq 0))))
(add-method #'sequence-get-right
  (method next-method ((seq <sequence>) . maybe-fk)
    (declare (ignore next-method))
    (if (collection-empty? seq)
        (and (pair? maybe-fk) (funcall (car maybe-fk)))
        (sequence-ref seq (- (collection-size seq) 1)))))

;;; - Flexible Sequences -

(define-function flexible-sequence? (class-predicate <flexible-sequence>))

(define-supertype-handled flexible-sequence?
  ((flexible-sequence-size fseq)
   collection-size)
  ((flexible-sequence-count fseq value)
   collection-count)
  ((flexible-sequence-get-any fseq . maybe-fk)
   collection-get-any)
  ((flexible-sequence-empty? fseq)
   collection-empty?)
  ((flexible-sequence->list fseq)
   collection->list)
  ((flexible-sequence-clear fseq)
   collection-clear)
  ((flexible-sequence-clear! fseq)
   collection-clear!)
  #|((flexible-sequence-copy fseq) ???
   collection-copy)|#

  ((flexible-sequence-comparator fseq)
   bag-comparator)
  ((flexible-sequence-contains? fseq value)
   bag-contains?)
  ((flexible-sequence-delete fseq value)
   bag-delete)
  ((flexible-sequence-delete! fseq value)
   bag-delete!)
  ((flexible-sequence-delete-from fseq value)
   bag-delete-from)
  ((flexible-sequence-delete-from! fseq value)
   bag-delete-from!)
  ((flexible-sequence-delete-all fseq value)
   bag-delete-all)
  ((flexible-sequence-delete-all! fseq value)
   bag-delete-all!)
  ((flexible-sequence-delete-all-from fseq bag)
   sequence-delete-all-from)
  ((flexible-sequence-delete-all-from! fseq bag)
   sequence-delete-all-from!)
  ((flexible-sequence-ref fseq k)
   sequence-ref)
  ((flexible-sequence-get-left fseq)
   sequence-get-left)
  ((flexible-sequence-get-right fseq)
   sequence-get-right)
  ((flexible-sequence-set fseq k value)
   sequence-set)
  ((flexible-sequence-set! fseq k value)
   sequence-set!)
  ((flexible-sequence-insert-right fseq value)
   sequence-insert-right)
  ((flexible-sequence-insert-right! fseq value)
   sequence-insert-right!)
  ((flexible-sequence-add fseq value)
   sequence-add)
  ((flexible-sequence-add! fseq value)
   sequence-add!)
  ((flexible-sequence-add-from fseq bag)
   sequence-add-from)
  ((flexible-sequence-add-from! fseq bag)
   sequence-add-from!)
  ((flexible-sequence-replace-from fseq dstart source . sstart+send)
   sequence-replace-from)
  ((flexible-sequence-replace-from! fseq dstart source . sstart+send)
   sequence-replace-from!)
  ((flexible-sequence-copy fseq . start+end)
   sequence-copy))

(define-function (flexible-sequence= elt=? . flexible-sequences)
  (for-each (lambda (flexible-sequence) (check-arg #'flexible-sequence?
                                                   flexible-sequence
                                                   'flexible-sequence=))
            flexible-sequences)
  (apply #'sequence= elt=? flexible-sequences))

(define-constrained-generics <flexible-sequence>
  flexible-sequence-insert
  flexible-sequence-insert!
  flexible-sequence-delete-at
  flexible-sequence-delete-at!
  flexible-sequence-insert-left
  flexible-sequence-insert-left!
  flexible-sequence-delete-left
  flexible-sequence-delete-left!
  flexible-sequence-delete-right
  flexible-sequence-delete-right!)

;;; - Maps -

(define-function map? (class-predicate <map>))

(define-supertype-handled map?
  ((map-size map)
   collection-size)
  ((map-count map value)
   collection-count)
  ((map-get-any map . athunk)
   collection-get-any)
  ((map-empty? map)
   collection-empty?)
  ((map->list map)
   collection->list)
  ((map-clear map)
   collection-clear)
  ((map-clear! map)
   collection-clear!)
  ((map-copy map)
   collection-copy))

(define-function (bag= elt=? . bags)
  (for-each (lambda (bag)
              (check-arg #'bag? bag 'bag=)) bags)
  (apply #'collection= elt=? bags))

(define-function (sequence= elt=? . sequences)
  (for-each (lambda (sequence)
              (check-arg #'sequence? sequence 'sequence=)) sequences)
  (apply #'collection= elt=? sequences))

(define-function (map= elt=? . maps)
  (for-each (lambda (map) (check-arg #'map? map 'map=)) maps)
  (apply #'collection= elt=? maps))


(define-constrained-generics <map>
  map-comparator
  map-key-comparator
  map-contains-key?
  map-keys->list
  map-get
  map-put map-put!
  map-update map-update!
  map-delete map-delete!
  map-delete-from map-delete-from!
  map-add-from map-add-from!)

;;;;;; - Scheme Collections -

;;; - Lists -

(define-function (make-list . maybe-size+fill)
  (let*-optionals #'make-list maybe-size+fill
      ((size 0) (fill :undef))
    (srfi-5:let loop ((l '()) (i 0))
      (if (= i size)
          l
          (loop (cons fill l) (+ i 1))))))

;; Auxiliaries

;; LIST is defined by R5RS.
; (define (list . l) l)

(add-method #'collection-name
  (method next-method ((l <list>)) 'list))

(define-function (list-fold-keys-left lst kons . knils)
  (let ((knil-count (list-size knils)))
    (iterate loop ((knils knils) (lst lst) (k 0))
      (list-case lst
        (lambda () (apply #'values knils))
        (lambda (elt1 elt2+)
          (receive (proceed? . new-knils)
              (apply kons k elt1 knils)
            (cond ((not (= (list-size new-knils) knil-count))
                   (error "Wrong number of knils"
                          #'list-fold-keys-left
                          `(expected ,knil-count)
                          `(got ,new-knils)))
                  (proceed?
                   (loop new-knils elt2+ (+ k 1)))
                  (:else
                   (apply #'values new-knils)))))))))

(define-function (list-fold-keys-right lst kons . knils)
  (let ((knil-count (list-size knils)))
    (call-with-values
      (lambda ()
        (iterate recur ((knils knils) (lst lst) (k 0))
          (list-case lst
            (lambda () (apply #'values 'T knils))
            (lambda (elt1 elt2+)
              (receive (proceed? . new-knils)
                  (recur knils elt2+ (+ k 1))
                (cond ((not (= (list-size new-knils) knil-count))
                       (error "Wrong number of knils"
                              #'list-fold-keys-right
                              `(expected ,knil-count)
                              `(got ,new-knils)))
                      (proceed?
                       (apply kons k elt1 new-knils))
                      (:else
                       (apply #'values 'NIL new-knils))))))))
      (lambda (_proceed? . vals)
        (declare (ignore _proceed?))
        (apply #'values vals)))))

(define-function (list-fold-left lst kons . knils)
  (apply #'list-fold-keys-left
         lst
         (lambda (index value . knils)
           (declare (ignore index))
           (apply kons value knils))
         knils))

(define-function (list-fold-right lst kons . knils)
  (apply #'list-fold-keys-right
         lst
         (lambda (index value . knils)
           (declare (ignore index))
           (apply kons value knils))
         knils))

(define-function (list-comparator l)
  (declare (ignore l))
  #'eqv?)

;; LIST-COPY may be defined by SRFI 1, but we extend it here to accept
;; the optional START and END arguments.
(define-function (list-copy lst . start+end)
  ;; Don't use LET*-OPTIONALS for finer control of what goes on here.
  (cond ((null? start+end)
         (list-fold-right lst (lambda (v knil)
                                (values 'T (cons v knil))) '()))
        ((null? (cdr start+end))
         (list-fold-right (drop lst (car start+end))
                          (lambda (v knil)
                            (values 'T (cons v knil))) '()))
        ((null? (cddr start+end))
         (let ((start (car start+end))
               (end (cadr start+end)))
           (take (drop lst start) (- end start))))
        (:else
         (apply #'error "list-copy: Too many arguments" lst start+end))))


(define-function (list->list lst) (list-copy lst))

;; LIST? is defined by R5RS.  This definition is stolen from SRFI 1; it
;; detects circular lists.
; (define (list? x)
;   (iterate loop ((x x) (lag x))
;     (if (pair? x)
;         (let ((x* (cdr x)))
;           (if (pair? x*)
;               (let ((x**  (cdr x*))
;                     (lag* (cdr lag)))
;                 (and (not (eq? x** lag))
;                      (loop x** lag*)))))
;         (null? x))))

(define-function list-size #'length)

(define-function (list-empty? l)
  (cond ((null? l) 'T)
        ((pair? l) 'NIL)
        (:else (error "Not a list" l))))

(define-function (list-contains? lst value)
  (and (memv value lst) 'T))

(define-function (list-ref lst index . maybe-ft)
  (let*-optionals #'list-ref maybe-ft
      ((ft (lambda () (error "list-ref: Index out of bounds"
                             lst index))))
    (if (< index 0)
        (funcall ft)
        (iterate loop ((l lst) (k index))
          (list-case l
            ft
            (lambda (elt1 elt2+)
              (if (zero? k)
                  elt1
                  (loop elt2+ (- k 1)))))))))

(define-function (list-set lst index value)
  (srfi-5:let recur ((l lst) (k index))
    (cond ((null? l)
           (error "List too short to set value at index"
                  lst index value))
          ((zero? k)
           (cons value (cdr l)))
          (:else
           (cons (car l) (recur (cdr l) (- k 1)))))))

(define-function (list-set! lst index value)
  (iterate loop ((l lst) (k index))
    (cond ((null? l)
           (error "List too short to set value at index"
                  lst index value))
          ((zero? k)
           (set-car! l value)
           lst)
          (:else
           (loop (cdr l) (- k 1))))))

(define-function (list-get-any lst . maybe-ft)
  (let*-optionals #'list-get-any maybe-ft
      ((ft (lambda () (error "list-get-any: Empty list"))))
    (list-case lst ft (lambda (a d) (declare (ignore d)) a))))
(define-function (list-get-left lst . maybe-ft)
  (let*-optionals #'list-get-left maybe-ft
      ((ft (lambda () (error "list-get-left: Empty list"))))
    (list-case lst ft (lambda (a d) (declare (ignore d)) a))))
(define-function (list-get-right lst . maybe-ft)
  (let*-optionals #'list-get-right maybe-ft
      ((ft (lambda () (error "list-get-right: Empty list"))))
    (if (list-empty? lst)
        (funcall ft)
        (iterate loop ((l lst))
          (if (list-empty? (cdr l))
              (car l)
              (loop (cdr l)))))))

(define-function (list-count lst value)
  (list-fold-left lst
    (lambda (elt count)
      (values 'T (if (eqv? elt value) (+ count 1) count)))
    0))

(define-function (list= elt=? . lists)
  (or (null? lists)
      (null? (cdr lists))
      (let* ((l1  (car  lists))
             (l2+ (cdr  lists))
             (l2  (car  l2+)))
        (and (binary-list= elt=? l1 l2)
             (apply #'list=  elt=? l2+)))))
(define-function (binary-list= elt=? l1 l2)
  (list-case l1
    (lambda () (list-empty? l2))
    (lambda (car1 cdr1)
      (list-case l2
        (lambda () 'NIL)
        (lambda (car2 cdr2)
          (and (funcall elt=? car1 car2)
               (binary-list= elt=? cdr1 cdr2)))))))

(define-function (list-add  lst value) (cons value lst))
;; A fresh cons cell would be allocated anyways, so there's no point in
;; making these two any different: LIST-ADD is faster in speed and just
;; as efficient in space than imperative LIST-ADD!.
(define-function list-add! #'list-add)
; (define (list-add! lst value)
;   (if (list-empty? lst)
;       (cons value lst)
;       (let ((new-tail (cons (car lst) (cdr lst))))
;         (set-car! lst value)
;         (set-cdr! lst new-tail)
;         lst)))

(define-function list-insert-left   #'list-add)
(define-function list-insert-left!  #'list-add!)

(define-function (list-insert-right lst value)
  (list-case lst
    (lambda () (list value))
    (lambda (x1 x2+)
      (cons x1 (list-insert-right x2+ value)))))
(define-function (list-insert-right! lst value)
  (if (list-empty? lst)
      (list value)
      (begin
        (iterate loop ((l lst))
          (if (null? (cdr l))
              (set-cdr! l (list value))
              (loop (cdr l))))
        lst)))

(define-function (list-delete lst value)
  (list-case lst
    (lambda () '())
    (lambda (elt1 elt2+)
      (if (eqv? elt1 value)
          elt2+
          (cons elt1 (list-delete elt2+ value))))))
(define-function (list-delete! lst value)
  (cond ((list-empty? lst)
         '())
        ((list-empty? (cdr lst))
         (if (eqv? (car lst) value)
             '()
             lst))
        (:else
         (iterate loop ((l (cdr lst)) (lag lst))
           (list-case l
             (lambda () lst)
             (lambda (elt1 elt2+)
               (if (eqv? elt1 value)
                   (begin (set-cdr! lag elt2+)
                          lst)
                   (loop elt2+ l))))))))

(define-function (list-delete-left lst)
  (list-case lst
    (lambda () (error "Can't delete left value from empty list"))
    (lambda (elt rest)
      (values rest elt))))
(define-function (list-delete-left! lst)
  (list-case lst
    (lambda () (error "Can't delete left value from empty list"))
    (lambda (elt1 elt2+)
      (values (if (list-empty? elt2+)
                  '()
                  (begin (set-car! lst (car elt2+))
                         (set-cdr! lst (cdr elt2+))
                         lst))
              elt1))))

(define-function (list-delete-right lst)
  (if (list-empty? lst)
      (error  "Can't delete right value from empty list")
      (srfi-5:let recur ((l lst))
        (if (list-empty? (cdr l))
            (values '() (car l))
            (receive (mumble last)
                (recur (cdr l))
              (values (cons (car l) mumble)
                      last))))))
(define-function (list-delete-right! lst)
  (cond ((list-empty? lst)
         (error "Can't delete right value from empty list"))
        ((list-empty? (cdr lst))
         (values '() (car lst)))
        (:else
         (iterate loop ((l (cdr lst)) (lag lst))
           (if (list-empty? (cdr l))
               (let ((elt (car l)))
                 (set-cdr! lag '())
                 (values lst elt))
               (loop (cdr l) l))))))

(define-function (list-delete-all lst value)
  (list-case lst
    (lambda () '())
    (lambda (elt1 elt2+)
      (if (eqv? elt1 value)
          (list-delete-all elt2+ value)
          (cons elt1 (list-delete-all elt2+ value))))))
(define-function (list-delete-all! lst value)
  (cond ((list-empty? lst)
         '())
        ((list-empty? (cdr lst))
         (if (eqv? (car lst) value)
             '()
             lst))
        (:else
         (iterate loop ((l (cdr lst)) (lag lst))
           (list-case l
             (lambda () lst)
             (lambda (elt1 elt2+)
               (if (eqv? elt1 value)
                   (begin (set-cdr! lag elt2+)
                          (loop elt2+ lag))
                   (loop (cdr l) l))))))))

(define-function (list-add-from lst bag)
  (collection-fold-left bag
    (lambda (elt l) (values 'T (list-add l elt)))
    lst))
(define-function (list-add-from! lst bag)
  (collection-fold-left bag
    (lambda (elt l) (values 'T (list-add! l elt)))
    lst))

(define-function (list-delete-from lst bag)
  (collection-fold-left bag
    (lambda (elt l) (values 'T (list-delete l elt)))
    lst))
(define-function (list-delete-from! lst bag)
  (collection-fold-left bag
    (lambda (elt l) (values 'T (list-delete! l elt)))
    lst))

(define-function (list-delete-all-from lst bag)
  (collection-fold-left bag
    (lambda (elt l) (values 'T (list-delete-all l elt)))
    lst))
(define-function (list-delete-all-from! lst bag)
  (collection-fold-left bag
    (lambda (elt l) (values 'T (list-delete-all! l elt)))
    lst))

(define-function (list-replace-from target tstart source . sstart+send)
  (let*-optionals #'list-replace-from sstart+send
      ((sstart 0) (send (list-size source)))
    (srfi-5:let recur ((l target) (i 0))
      (if (= i tstart)
          (append (list-copy source sstart send)
                  (drop (- send sstart) l))
          (list-case l
            (lambda ()
              (error "List too short to replace elements"
                     target tstart))
            (lambda (a d)
              (cons a (recur d (+ i 1)))))))))

(define-function (list-replace-from! target tstart source . sstart+send)
  (let*-optionals #'list-replace-from sstart+send
      ((sstart 0) (send (list-size source)))
    (do ((\t (drop target tstart) (cdr \t))
         (s (drop source sstart) (cdr s))
         (i sstart (+ i 1)))
        ((= i send)
         target)
      (set-car! \t (car s)))))

(define-function (list-clear lst)
  (declare (ignore lst))
  '())
(define-function (list-clear! lst)
  ;; We have no choice but to leave LST as a pair, but we can at least
  ;; clear out the CAR & CDR, so that they may become garbage.
  (set-car! lst 'NIL)
  (set-cdr! lst '())
  '())

(define-function (list-insert lst index value)
  (flet ((die ()
           (error "List too short to insert value at index"
                  lst value index)))
    (srfi-5:let recur ((l lst) (k index))
      (if (zero? k)
          (cons value l)
          (list-case l #'die
                     (lambda (a d)
                       (cons a (recur d (- k 1))) ))))))
(define-function (list-insert! lst index value)
  (flet ((die ()
           (error "List too short to insert value at index"
                  lst value index)))
    (cond ((zero? index) (cons value lst))
          ((list-empty? lst) (die))
          (:else
           (iterate loop ((lag lst) (index (- index 1)))
                    (cond ((null? lag) (die))
                          ((zero? index)
                           (set-cdr! lag (cons value (cdr lag)))
                           lst )
                          (:else
                           (loop (cdr lag) (- index 1)) )))))))

(define-function (list-delete-at lst index)
  (flet ((die ()
           (error "List too short to delete value at index"
                  lst index)))
  (if (list-empty? lst)
      (die)
      (srfi-5:let recur ((l lst) (k index))
        (list-case l #'die
          (if (zero? k)
              (lambda (a d) (declare (ignore a)) d)
              (lambda (a d)
                (cons a (recur d (- k 1))))))))))
(define-function (list-delete-at! lst index)
  (flet ((die ()
           (error "List too short to delete value at index"
                  lst index)))
    (cond ((zero? index) (cdr lst))
          ((list-empty? lst) (die))
          (:else
           (iterate loop ((lag lst) (index (- index 1)))
                    (cond ((null? lag) (die))
                          ((zero? index)
                           (if (null? (cdr lag))
                               (die) )
                           (set-cdr! lag (cddr lag))
                           lst )
                          (:else
                           (loop (cdr lag) (- index 1)) )))))))

(add-methods
 (list #'collection-fold-keys-left  #'list-fold-keys-left  (list <list>))
 (list #'collection-fold-keys-right #'list-fold-keys-right (list <list>))

 (list #'collection-fold-left  #'list-fold-left  (list <list>))
 (list #'collection-fold-right #'list-fold-right (list <list>))

 (list #'collection-size   #'list-size   (list <list>))
 (list #'collection-count  #'list-count  (list <list>))
 (list #'collection-empty? #'list-empty? (list <list>))

 (list #'collection-get-any #'list-get-any (list <list>))

 (list #'collection-copy  #'list-copy  (list <list>))

 (list #'collection-clear  #'list-clear  (list <list>))
 (list #'collection-clear! #'list-clear! (list <list>))

 (list #'collection= #'list= (list <top> <list>))

 ;;;

 (list #'bag-comparator #'list-comparator (list <list>))

 (list #'bag-contains? #'list-contains? (list <list>))

 (list #'bag-add  #'list-add  (list <list> <top>))
 (list #'bag-add! #'list-add! (list <list> <top>))

 (list #'bag-delete  #'list-delete  (list <list> <top>))
 (list #'bag-delete! #'list-delete! (list <list> <top>))

 (list #'bag-delete-all  #'list-delete-all  (list <list> <top>))
 (list #'bag-delete-all! #'list-delete-all! (list <list> <top>))

 (list #'bag-add-from  #'list-add-from  (list <list> <bag>))
 (list #'bag-add-from! #'list-add-from! (list <list> <bag>))

 (list #'bag-delete-from  #'list-delete-from  (list <list> <bag>))
 (list #'bag-delete-from! #'list-delete-from! (list <list> <bag>))

 (list #'bag-delete-all-from  #'list-delete-all-from  (list <list> <bag>))
 (list #'bag-delete-all-from! #'list-delete-all-from! (list <list> <bag>))

 ;;;

 (list #'sequence-ref  #'list-ref  (list <list> <number>))
 (list #'sequence-set  #'list-set  (list <list> <number>))
 (list #'sequence-set! #'list-set! (list <list> <number>))

 (list #'sequence-get-left  #'list-get-left  (list <list>))
 (list #'sequence-get-right #'list-get-right (list <list>))

 (list #'sequence-replace-from  #'list-replace-from
       (list <list> <number> <list>))
 (list #'sequence-replace-from! #'list-replace-from!
       (list <list> <number> <list>))

 (list #'sequence-copy #'list-copy (list <list>))

 ;;;

 (list #'flexible-sequence-insert  #'list-insert  (list <list> <number>))
 (list #'flexible-sequence-insert! #'list-insert! (list <list> <number>))

 (list #'flexible-sequence-delete-at  #'list-delete-at
       (list <list> <number>))
 (list #'flexible-sequence-delete-at! #'list-delete-at!
       (list <list> <number>))

 (list #'flexible-sequence-insert-left  #'list-insert-left  (list <list> <top>))
 (list #'flexible-sequence-insert-left! #'list-insert-left! (list <list> <top>))

 (list #'sequence-insert-right  #'list-insert-right
       (list <list> <top>))
 (list #'sequence-insert-right! #'list-insert-right!
       (list <list> <top>))

 (list #'flexible-sequence-delete-left  #'list-delete-left  (list <list>))
 (list #'flexible-sequence-delete-left! #'list-delete-left! (list <list>))

 (list #'flexible-sequence-delete-right  #'list-delete-right
       (list <list>))
 (list #'flexible-sequence-delete-right! #'list-delete-right!
       (list <list>)))

;;; - Alists -

(add-method #'initialize
  (method next-method ((lmap <alist-map>) initargs)
    (slot-set! lmap 'comparator (car initargs))
    (slot-set! lmap 'contents   (cons (list (list 'unique))
                                      (cadr initargs)))
    lmap))

(add-method #'collection-name
  (method next-method ((lmap <alist-map>)) 'alist-map))

(define-function (alist-map= elt=? . maps)
  (for-each (lambda (map) (check-arg #'alist-map? map 'alist-map=)) maps)
  (apply #'map= elt=? maps))

(define-function (make-alist-map . maybe-key-comparator)
  (let*-optionals #'make-alist-map maybe-key-comparator
                  ((key-comparator #'eqv?))
    (make <alist-map> key-comparator '())))
(define-function (copy-pair pair)
  (cons (car pair) (cdr pair)))
(define-function (alist-map . args)
  (cond ((null? args)
         (make <alist-map> #'eqv? '()))
        ((procedure? (car args))
         (make <alist-map> (car args) (map #'copy-pair (cdr args))))
        (:else
         (make <alist-map> #'eqv? (map #'copy-pair args)))))

(define-function alist-map? (make-generic-predicate <alist-map>))

(add-method #'collection-name
  (method next-method ((l <alist-map>)) 'alist-map))

(define-syntax destructure-alist-map
  (syntax-rules ()
    ((destructure-alist-map ?lmap ('NIL 'NIL 'NIL)
       ?body1 ?body2 ***)
     (let () ?body1 ?body2 ***))
    ((destructure-alist-map ?lmap (?compare 'NIL 'NIL)
       ?body1 ?body2 ***)
     (let ((?compare (slot-ref ?lmap 'comparator)))
       ?body1 ?body2 ***))
    ((destructure-alist-map ?lmap ('NIL ?contents 'NIL)
       ?body1 ?body2 ***)
     (let ((?contents (slot-ref ?lmap 'contents)))
       ?body1 ?body2 ***))
    ((destructure-alist-map ?lmap (?compare ?contents 'NIL)
       ?body1 ?body2 ***)
     (let ((?compare (slot-ref ?lmap 'comparator))
           (?contents (slot-ref ?lmap 'contents)))
       ?body1 ?body2 ***))
    ((destructure-alist-map ?lmap ('NIL 'NIL ?contents-cdr)
       ?body1 ?body2 ***)
     (let ((?contents-cdr (cdr (slot-ref ?lmap 'contents))))
       ?body1 ?body2 ***))
    ((destructure-alist-map ?lmap (?compare 'NIL ?contents-cdr)
       ?body1 ?body2 ***)
     (let ((?compare (slot-ref ?lmap 'comparator))
           (?contents-cdr (cdr (slot-ref ?lmap 'contents))))
       ?body1 ?body2 ***))
    ((destructure-alist-map ?lmap ('NIL ?contents ?contents-cdr)
       ?body1 ?body2 ***)
     (let ((?contents (slot-ref ?lmap 'contents))
           (?contents-cdr (cdr (slot-ref ?lmap 'contents))))
       ?body1 ?body2 ***))
    ((destructure-alist-map ?lmap (?compare ?contents ?contents-cdr)
       ?body1 ?body2 ***)
     (let ((?compare (slot-ref ?lmap 'comparator))
           (?contents (slot-ref ?lmap 'contents))
           (?contents-cdr (cdr (slot-ref ?lmap 'contents))))
       ?body1 ?body2 ***))))

(define-function (alist-map-fold-keys-left lmap kons . knils)
  (destructure-alist-map lmap ('NIL 'NIL contents)
    (apply #'list-fold-left
           contents
           (lambda (association . knils*)
             (apply kons (car association) (cdr association) knils*))
           knils)))
(define-function (alist-map-fold-keys-right lmap kons . knils)
  (destructure-alist-map lmap ('NIL 'NIL contents)
    (apply #'list-fold-right
           contents
           (lambda (association . knils*)
             (apply kons (car association) (cdr association) knils*))
           knils)))

(define-function (alist-map-fold-left lmap kons . knils)
  (apply #'alist-map-fold-keys-left lmap
         (lambda (key value . knils)
           (declare (ignore key))
           (apply kons value knils))
         knils))
(define-function (alist-map-fold-right lmap kons . knils)
  (apply #'alist-map-fold-keys-right lmap
         (lambda (key value . knils)
           (declare (ignore key))
           (apply kons value knils))
         knils))

(define-function (alist-map-key-comparator lmap)
  (slot-ref lmap 'comparator))
(define-function alist-map-comparator #'alist-map-key-comparator)

(define-function (alist-map-count lmap value)
  (alist-map-fold-left lmap
    (lambda (value* count)
      (values 'T (if (eqv? value* value) (+ count 1) count)))
    0))
(define-function (alist-map-key-count lmap key)
  (destructure-alist-map lmap (compare 'NIL 'NIL)
    (alist-map-fold-keys-left lmap
      (lambda (key* _value count)
        (declare (ignore _value))
        (values 'T (if (funcall compare key* key) (+ count 1) count)))
      0)))


(define-function (alist-map-get-any lmap . maybe-ft)
  (let*-optionals #'alist-map-get-any maybe-ft
      ((ft (lambda () 'NIL)))
    (destructure-alist-map lmap ('NIL 'NIL contents)
      (if (null? contents)
          (funcall ft)
          (cdar contents)))))

(add-method #'collection-get-any
  (method next-method ((map <map>) . maybe-ft)
    (declare (ignore next-method))
    (apply #'alist-map-get-any map maybe-ft)))

(define-function (alist-map-contains-key? lmap key)
  (destructure-alist-map lmap (compare 'NIL contents)
    (declare (ignore contents))
    (alist-map-fold-keys-left lmap
      (lambda (key* _value mumble)
        (declare (ignore _value mumble))
        (if (funcall compare key* key)
            (values 'NIL 'T)
            (values 'T 'NIL)))
      'NIL)))

(define-function (alist-map-size lmap)
  (length (cdr (slot-ref lmap 'contents))))
(define-function (alist-map-empty? lmap)
  (null? (cdr (slot-ref lmap 'contents))))

(define-function (alist-map-copy lmap)
  (destructure-alist-map lmap (compare 'NIL contents)
    (make <alist-map> compare
          ;Must use a deep copy here
          (map #'copy-pair contents))))
(define-function (alist-map->list lmap)
  (map #'cdr (cdr (slot-ref lmap 'contents))))
(define-function (alist-map-keys->list lmap)
  (map #'car (cdr (slot-ref lmap 'contents))))

(define-function (alist-map-clear lmap)
  (make <alist-map> (slot-ref lmap 'comparator) '()))
(define-function (alist-map-clear! lmap)
  (set-cdr! (slot-ref lmap 'contents) '())
  ;(slot-set! lmap 'contents (list (list 'unique)))
  lmap)

(define-function alist-map? (make-generic-predicate <alist-map>))

(define-function (alist-map-get lmap key . maybe-ft)
  (let*-optionals #'alist-map-get maybe-ft
      ((ft (lambda () 'NIL)))
    (destructure-alist-map lmap (compare 'NIL contents)
      (iterate loop ((l contents))
        (cond ((null? l)
               (funcall ft))
              ((funcall compare (caar l) key)
               (cdar l))
              (:else
               (loop (cdr l))))))))

(define-function (alist-map-put lmap key value . maybe-ft)
  (let*-optionals #'alist-map-put maybe-ft
      ((ft (lambda () 'NIL)))
    (destructure-alist-map lmap (compare 'NIL contents)
      (receive (contents prv)
        (srfi-5:let recur ((l contents))
          (cond ((null? l)
                 (values (list (cons key value)) (funcall ft)))
                ((funcall compare (caar l) key)
                 (values (cons (cons key value) (cdr l))
                         (cdar l)))
                (:else
                 (receive (tail value)
                     (recur (cdr l))
                   (values (cons (car l) tail) value)))))

        (values (make <alist-map> compare contents) prv)))))

(define-function (alist-map-put! lmap key value . maybe-ft)
  (let*-optionals #'alist-map-put! maybe-ft
      ((ft (lambda () 'NIL)))
    (destructure-alist-map lmap (compare lag l)
      (iterate loop ((l l) (lag lag))
        (cond ((null? l)
               (set-cdr! lag (list (cons key value)))
               (values lmap (funcall ft)))
              ((funcall compare (caar l) key)
               (let ((old (cdar l)))
                 (set-cdr! (car l) value)
                 (values lmap old)))
              (:else
               (loop (cdr l) l)))))))

(define-function (alist-map-update lmap key f . maybe-dt)
  (let*-optionals #'alist-map-update maybe-dt
      ((dt (lambda () 'NIL)))
    (destructure-alist-map lmap (compare 'NIL contents)
      (make <alist-map> compare
        (srfi-5:let recur ((l contents))
          (cond ((null? l)
                 (list (cons key (funcall f (funcall dt)))))
                ((funcall compare (caar l) key)
                 (cons (cons key (funcall f (cdar l))) (cdr l)))
                (:else
                 (cons (car l) (recur (cdr l))))))))))
(define-function (alist-map-update! lmap key f . maybe-dt)
  (let*-optionals #'alist-map-update! maybe-dt
      ((dt (lambda () 'NIL)))
    (destructure-alist-map lmap (compare lag l)
      (iterate loop ((l l) (lag lag))
        (cond ((null? l)
               (set-cdr! lag (list (cons key (funcall f (funcall dt)))))
               lmap)
              ((funcall compare (caar l) key)
               (set-cdr! (car l) (funcall f (cdar l)))
               lmap)
              (:else
               (loop (cdr l) l)))))))

(define-function (alist-map-delete lmap key)
  (destructure-alist-map lmap (compare 'NIL contents)
    (make <alist-map> compare
      (srfi-5:let recur ((l contents))
        (cond ((null? l)
               '())
              ((funcall compare (caar l) key)
               (cdr l))
              (:else
               (cons (car l) (recur (cdr l)))))))))
(define-function (alist-map-delete! lmap key)
  (destructure-alist-map lmap (compare lag l)
    (iterate loop ((l l) (lag lag))
      (cond ((null? l)
             lmap)
            ((funcall compare (caar l) key)
             (set-cdr! lag (cdr l))
             lmap)
            (:else
             (loop (cdr l) l))))))

(define-function (alist-map-delete-from lmap bag)
  (collection-fold-left bag
    (lambda (key lmap*)
      (values 'T (alist-map-delete lmap* key)))
    lmap))
(define-function (alist-map-delete-from! lmap bag)
  (collection-fold-left bag
    (lambda (key lmap*)
      (values 'T (alist-map-delete! lmap* key)))
    lmap))

(define-function (alist-map-add-from lmap-target lmap-source)
  (alist-map-fold-keys-left lmap-source
    (lambda (key value target)
      (multiple-value-bind (newmap oldval)
                           (alist-map-put target key value)
        (declare (ignore oldval))
        (values 'T newmap)))
    lmap-target))
(define-function (alist-map-add-from! lmap-target lmap-source)
  (alist-map-fold-keys-left lmap-source
    (lambda (key value target)
      (multiple-value-bind (newmap oldval)
                           (alist-map-put target key value)
        (declare (ignore oldval))
        (values 'T newmap)))
    lmap-target))


(add-methods
 (list #'collection-fold-left  #'alist-map-fold-left  (list <alist-map>))
 (list #'collection-fold-right #'alist-map-fold-right (list <alist-map>))

 (list #'collection-fold-keys-left  #'alist-map-fold-keys-left
       (list <alist-map>))
 (list #'collection-fold-keys-right #'alist-map-fold-keys-right
       (list <alist-map>))

 (list #'collection-size #'alist-map-size (list <alist-map>))
 (list #'collection-count #'alist-map-count (list <alist-map>))

 (list #'map-comparator     #'alist-map-comparator     (list <alist-map>))
 (list #'map-key-comparator #'alist-map-key-comparator (list <alist-map>))

 (list #'map-contains-key? #'alist-map-contains-key? (list <alist-map>))

 (list #'map-keys->list #'alist-map-keys->list (list <alist-map>))

 (list #'map-get #'alist-map-get (list <alist-map>))

 (list #'collection-clear #'alist-map-clear (list <alist-map>))
 (list #'collection-clear! #'alist-map-clear! (list <alist-map>))
 (list #'collection-clear #'alist-map-clear (list <alist-map>))
 (list #'collection-clear! #'alist-map-clear! (list <alist-map>))

 (list #'collection-copy  #'alist-map-copy  (list <alist-map>))

 (list #'map-put  #'alist-map-put  (list <alist-map>))
 (list #'map-put! #'alist-map-put! (list <alist-map>))

 (list #'map-update  #'alist-map-update  (list <alist-map>))
 (list #'map-update! #'alist-map-update! (list <alist-map>))

 (list #'map-delete  #'alist-map-delete  (list <alist-map>))
 (list #'map-delete! #'alist-map-delete! (list <alist-map>))

 (list #'map-delete-from  #'alist-map-delete-from  (list <alist-map>))
 (list #'map-delete-from! #'alist-map-delete-from! (list <alist-map>))

 (list #'map-add-from  #'alist-map-add-from  (list <alist-map>))
 (list #'map-add-from! #'alist-map-add-from! (list <alist-map>)))

;;; - Vectors and Strings -

;; MAKE-VECTOR, MAKE-STRING, VECTOR, and STRING? are R5RS primitives.

;(define (vector . elts) (list->vector elts)) ;R5RS
;(define (string . elts) (list->string elts)) ;R5RS

(add-method #'collection-name (method next-method ((v <vector>)) 'vector))
(add-method #'collection-name (method next-method ((s <string>)) 'string))

(define-function vector-size #'vector-length)
(define-function string-size #'string-length)

(define-function (vector-folder vector-ref select-start select-end select-next)
  (letrec
      ((fold
        (lambda (seq kons . knils)
          (let ((knil-count (list-size knils))
                (start (funcall select-start seq))
                (end (funcall select-end seq)))
            (iterate loop ((knils knils) (i start))
              (if (= i end)
                  (apply #'values knils)
                  (receive (proceed? . new-knils)
                      (apply kons i (funcall vector-ref seq i) knils)
                    (cond ((not (= (list-size new-knils) knil-count))
                           (error "Too many knils"
                                  fold
                                  `(expected ,knil-count)
                                  `(got ,new-knils)))
                          (proceed?
                           (loop new-knils (funcall select-next i)))
                          (:else
                           (apply #'values knils))))))))))
    fold))

(define-function vector-fold-keys-left
  (vector-folder #'vector-ref
    (always 0)             ;select-start
    #'vector-size            ;select-end
    (lambda (i) (+ i 1)))) ;select-next
(define-function vector-fold-keys-right
  (vector-folder #'vector-ref
    (lambda (vec)          ;select-start
      (- (vector-size vec) 1))
    (always -1)            ;select-end
    (lambda (i) (- i 1)))) ;select-next
(define-function (vector-fold-left vec kons . knils)
  (apply #'vector-fold-keys-left vec
         (lambda (index elt . knils)
           (declare (ignore index))
           (apply kons elt knils))
         knils))
(define-function (vector-fold-right vec kons . knils)
  (apply #'vector-fold-keys-right vec
         (lambda (index elt . knils)
           (declare (ignore index))
           (apply kons elt knils))
         knils))

(define-function string-fold-keys-left
  (vector-folder #'string-ref
    (always 0)               ;select-start
    #'string-size              ;select-end
    (lambda (i) (+ i 1)))) ;select-next
(define-function string-fold-keys-right
  (vector-folder #'string-ref
    (lambda (s)            ;select-start
      (- (string-size s) 1))
    (always -1)            ;select-end
    (lambda (i) (- i 1)))) ;select-next
(define-function (string-fold-left string kons . knils)
  (apply #'string-fold-keys-left string
         (lambda (index elt . knils)
           (declare (ignore index))
           (apply kons elt knils))
         knils))
(define-function (string-fold-right string kons . knils)
  (apply #'string-fold-keys-right string
         (lambda (index elt . knils)
           (declare (ignore index))
           (apply kons elt knils))
         knils))

(define-function (vector-comparator vec)
  (declare (ignore vec))
  #'eqv?)
(define-function (string-comparator str)
  (declare (ignore str))
  #'char=?)

(define-function (vector-empty? vector)
  (zero? (vector-size vector)))
(define-function (string-empty? string)
  (zero? (string-size string)))

(define-function (vlike-copier make-x x-size x-ref x-set!)
  (rec (copy vec . maybe-start+end)
    (let*-optionals #'copy maybe-start+end
        ((start 0) (end (funcall x-size vec)))
      (do ((new (funcall make-x (- end start)))
           (i start (+ i 1))
           (j 0 (+ j 1)))
          ((= i end) new)
        (funcall x-set! new j (funcall x-ref vec i))))))
(define-function vector-copy (vlike-copier #'make-vector #'vector-size
                                  #'vector-ref #'vector-set!))
(define-function string-copy (vlike-copier #'make-string #'string-size
                                  #'string-ref #'string-set!))

;; R5RS defines both VECTOR->LIST and STRING->LIST.

(define-function (vlike-contains? x-size x-ref elt=?)
  (lambda (vec value)
    (let ((size (funcall x-size vec)))
      (iterate loop ((i 0))
        (cond ((= i size)
               'NIL)
              ((funcall elt=? (funcall x-ref vec i) value)
               'T)
              (:else
               (loop (+ i 1))))))))
(define-function vector-contains?
  (vlike-contains? #'vector-size #'vector-ref #'eqv?))
(define-function string-contains?
  (vlike-contains? #'string-size #'string-ref #'char=?))

(define-function (vlike-count x-size x-ref elt=?)
  (lambda (vec value)
    (do ((size (funcall x-size vec))
         (i 0 (+ i 1))
         (c 0 (if (funcall elt=? (funcall x-ref vec i) value)
                  (+ c 1)
                  c)))
        ((= i size) c))))
(define-function vector-count (vlike-count #'vector-size #'vector-ref #'eqv?))
(define-function string-count (vlike-count #'string-size #'string-ref #'char=?))

(define-function (vlike-ref x-size x-ref)
  (rec (x-ref* vec k . maybe-ft)
    (cond ((null? maybe-ft)
           (funcall x-ref vec k))
          ((null? (cdr maybe-ft))
           (if (or (< k 0) (>= k (funcall x-size vec)))
               (funcall (car maybe-ft))
               (funcall x-ref vec k)))
          (:else
           (apply #'error "Too many arguments" x-ref*
                  vec k
                  maybe-ft)))))
(define-function vector-ref (vlike-ref #'vector-size #'vector-ref))
(define-function string-ref (vlike-ref #'string-size #'string-ref))

(define-function (vlike-get-left x-size x-ref)
  (rec (x-get-right vec . maybe-ft)
    (cond ((null? maybe-ft)
           (funcall x-ref vec 0))
          ((null? (cdr maybe-ft))
           (if (zero? (funcall x-size vec))
               (funcall (car maybe-ft))
               (funcall x-ref vec 0)))
          (:else
           (error "Too many arguments" x-get-right
                  vec
                  maybe-ft)))))
(define-function vector-get-left (vlike-get-left #'vector-size #'vector-ref))
(define-function string-get-left (vlike-get-left #'string-size #'string-ref))

(define-function vector-get-any #'vector-get-left)
(define-function string-get-any #'string-get-left)

(define-function (vlike-get-right x-size x-ref)
  (rec (x-get-right vec . maybe-ft)
    (cond ((null? maybe-ft)
           (funcall x-ref vec (- (funcall x-size vec) 1)))
          ((null? (cdr maybe-ft))
           (if (zero? (funcall x-size vec))
               (funcall (car maybe-ft))
               (funcall x-ref vec (- (funcall x-size vec) 1))))
          (:else
           (error "Too many arguments" x-get-right
                  vec
                  maybe-ft)))))
(define-function vector-get-right (vlike-get-right #'vector-size #'vector-ref))
(define-function string-get-right (vlike-get-right #'string-size #'string-ref))

(define-function (vlike-set x-copy x-set!)
  (lambda (vec k value)
    (let ((copy (funcall x-copy vec)))
      (funcall x-set! vec k value)
      copy)))
(define-function vector-set (vlike-set #'vector-copy #'vector-set!))
(define-function string-set (vlike-set #'string-copy #'string-set!))

(define-function (vector-clear  vec)
  (declare (ignore vec))
  (make-vector 0))
(define-function (vector-clear! vec)
  (vector-fill! vec 'NIL) (make-vector 0))

(define-function (string-clear s)
  (declare (ignore s))
  (make-string 0))
(define-function (string-clear! s)
  (declare (ignore s))
  (make-string 0))

(define-function (vlike-replace-from! x-size x-ref x-set!)
  (rec (x-replace-from! target tstart source . maybe-sstart+send)
    (let*-optionals #'x-replace-from! maybe-sstart+send
        ((sstart 0) (send (funcall x-size source)))
      ;; To hell with bounds checking.
      (do ((i tstart (+ i 1))
           (j sstart (+ j 1)))
          ((= j send))
        (funcall x-set! target i (funcall x-ref source j))))))
(define-function vector-replace-from!
  (vlike-replace-from! #'vector-size #'vector-ref #'vector-set!))
(define-function string-replace-from!
  (vlike-replace-from! #'string-size #'string-ref #'string-set!))

(define-function (vlike-replace-from x-copy x-replace-from!)
  (lambda (target tstart source . maybe-sstart+send)
    (let ((copy (funcall x-copy target)))
      (apply x-replace-from! copy tstart source maybe-sstart+send)
      copy)))
(define-function vector-replace-from
  (vlike-replace-from #'vector-copy #'vector-replace-from!))
(define-function string-replace-from
  (vlike-replace-from #'string-copy #'string-replace-from!))

(define-function (vlike= x-size x-ref)
  (flet ((binary= ( elt=? v1 v2)
           (let ((s1 (funcall x-size v1)))
             (and (= s1 (funcall x-size v2))
                  (iterate loop ((i 0))
                           (or (= i s1)
                               (and (funcall elt=?
                                             (funcall x-ref v1 i)
                                             (funcall x-ref v2 i))
                                    (loop (+ i 1)))))))))
    (rec (x= elt=? . vecs)
         (or (null? vecs) (null? (cdr vecs))
             (and (binary= elt=? (car vecs) (cadr vecs))
                  (apply x= elt=? (cdr vecs)))))))

(define-function vector= (vlike= #'vector-size #'vector-ref))
(define-function string= (vlike= #'string-size #'string-ref))

;Generic Vector methods
(add-methods
 (list #'collection-fold-keys-left  #'vector-fold-keys-left  (list <vector>))
 (list #'collection-fold-keys-right #'vector-fold-keys-right (list <vector>))

 (list #'collection-fold-left  #'vector-fold-left  (list <vector>))
 (list #'collection-fold-right #'vector-fold-right (list <vector>))

 (list #'collection-size   #'vector-size   (list <vector>))
 (list #'collection-count  #'vector-count  (list <vector>))
 (list #'collection-empty? #'vector-empty? (list <vector>))

 (list #'collection-get-any #'vector-get-any (list <vector>))

 (list #'collection-copy  #'vector-copy  (list <vector>))

 (list #'collection-clear  #'vector-clear  (list <vector>))
 (list #'collection-clear! #'vector-clear! (list <vector>))

 (list #'collection= #'vector= (list <top> <vector>))

 ;;;

 (list #'bag-comparator #'vector-comparator (list <vector>))

 (list #'bag-contains? #'vector-contains? (list <vector>))

 ;;;

 (list #'sequence-ref  #'vector-ref  (list <vector> <number>))
 (list #'sequence-set  #'vector-set  (list <vector> <number>))
 (list #'sequence-set! #'vector-set! (list <vector> <number>))

 (list #'sequence-get-left  #'vector-get-left  (list <vector>))
 (list #'sequence-get-right #'vector-get-right (list <vector>))

 (list #'sequence-replace-from  #'vector-replace-from
       (list <vector> <number> <vector>))
 (list #'sequence-replace-from! #'vector-replace-from!
       (list <vector> <number> <vector>))

 (list #'sequence-copy #'vector-copy (list <vector>)))

;Generic String Methods
(add-methods
 (list #'collection-fold-keys-left  #'string-fold-keys-left  (list <string>))
 (list #'collection-fold-keys-right #'string-fold-keys-right (list <string>))

 (list #'collection-fold-left  #'string-fold-left  (list <string>))
 (list #'collection-fold-right #'string-fold-right (list <string>))

 (list #'collection-size   #'string-size   (list <string>))
 (list #'collection-count  #'string-count  (list <string>))
 (list #'collection-empty? #'string-empty? (list <string>))

 (list #'collection-get-any #'string-get-any (list <string>))

 (list #'collection-copy  #'string-copy  (list <string>))

 (list #'collection-clear  #'string-clear  (list <string>))
 (list #'collection-clear! #'string-clear! (list <string>))

 (list #'collection= #'string= (list <top> <string>))

 ;;;

 (list #'bag-comparator #'string-comparator (list <string>))

 (list #'bag-contains? #'string-contains? (list <string>))

 ;;;
 (list #'sequence-ref  #'string-ref  (list <string> <number>))
 (list #'sequence-set  #'string-set  (list <string> <number>))
 (list #'sequence-set! #'string-set! (list <string> <number>))

 (list #'sequence-get-left  #'string-get-left  (list <string>))
 (list #'sequence-get-right #'string-get-right (list <string>))

 (list #'sequence-replace-from  #'string-replace-from
       (list <string> <number> <string>))
 (list #'sequence-replace-from! #'string-replace-from!
       (list <string> <number> <string>))

 (list #'sequence-copy #'string-copy (list <string>)))

;;; Local Variables: ***
;;; eval: (put 'destructure-alist-map 'scheme-indent-function 2) ***
;;; End: ***
