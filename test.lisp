(cl:in-package :srfi-44.internal)

(define-function (list-contents-equal? ls1 ls2)
  (and (= (length ls1) (length ls2))
       (let loop ((x ls1))
         (or (null? x)
             (and (member (car x) ls2)
                  (loop (cdr x)))))))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun == (x y)
    (typecase x
      (vector (equalp x y))
      (T (equal x y)) )))

(define-syntax test-clauses
  (syntax-rules (=> <>)
    ((test-clauses) (progn))
    ((test-clauses
      (num expr => result) rest ***)
     (progn
       (is (== result expr))
       (test-clauses rest ***)))
    ((test-clauses
      (num expr <> result) rest ***)
     (progn (is (not (== result expr)))
            (test-clauses rest ***)))
    ((test-clauses (num expr) rest ***)
     (progn (is-true (progn expr t))
            (test-clauses rest ***)))
    ((test-clauses rest ***)
     (progn rest ***))))

#|(syntax-rules (=> <>)
((test* name
            (num expr => result) ***)
     (test name
       (is (== result expr)) ***))
    ((test* name
            (num expr <> result) ***)
     (test name
       (is (not (== result expr))) ***))
    ((test* name
            (num expr) ***)
     (test name
       (is-true (progn expr t)) ***)))|#

(define-syntax test*
  (syntax-rules (=> <>)
    ((test* name expr => result)
     (test name
       (is (== result expr))))
    ((test* name expr <> result)
     (test name
       (is (not (== result expr)))))
    ((test* name clauses ***)
     (test name (test-clauses clauses ***)))))

(define-syntax *-
  (syntax-rules ()
    ((_ base-type (class op args ***))
     (test*-* 'class 'op 'base-type args ***))
    ((_ (class op args ***))
     (test*-* 'class 'op 'NIL args ***))))

(define-syntax test-help
  (syntax-rules ()
    ((_ (class op args ***))
     (funcall (intern (format nil "~a~a" 'class 'op))
              args ***))))

(define-syntax test-help/-
  (syntax-rules ()
    ((_ (class op args ***))
     (funcall (intern (format nil "~a-~a" 'class 'op))
              args ***))))

(define-syntax test-*
  (syntax-rules (list alist-map flexible-sequence sequence)
    ((_ id (list op args ***) result ***)
     (test* id
           (0  (test-help/- (list op args ***)) result ***)
           (1  (test-help/- (flexible-sequence op args ***)) result ***)
           (2  (test-help/- (sequence op args ***)) result ***)
           (3  (test-help/- (bag op args ***)) result ***)))
    ((_ id (alist-map op args ***) result ***)
     (test* id
           (0  (test-help/- (alist-map op args ***)) result ***)
           (1  (test-help/- (map op args ***)) result ***)))
    ((_ id (seq op args ***) result ***)
     (test* id (0  (test-help/- (seq op args ***)) result ***)
              (1  (test-help/- (sequence op args ***)) result ***)
              (2  (test-help/- (bag op args ***)) result ***)))
    ((_ id sequence (seq op args ***) result ***)
     (test* id (0  (test-help/- (seq op args ***)) result ***)
              (1  (test-help/- (sequence op args ***)) result ***)))
    ((_ id flexible-sequence (coll op args ***) result ***)
     (test* id
           (0  (test-help/- (coll op args ***)) result ***)
           (1  (test-help/- (flexible-sequence op args ***)) result ***)))))

(define-syntax testc-*
  (syntax-rules (list alist-map flexible-sequence sequence)
    ((_ id (list op args ***) result ***)
     (test* id
           (0  (test-help/- (list op args ***)) result ***)
           (1  (test-help/- (flexible-sequence op args ***)) result ***)
           (2  (test-help/- (sequence op args ***)) result ***)
           (3  (test-help/- (bag op args ***)) result ***)
           (4  (test-help/- (collection op args ***)) result ***)))
    ((_ id (alist-map op args ***) result ***)
     (test* id
           (0  (test-help/- (alist-map op args ***)) result ***)
           (1  (test-help/- (map op args ***)) result ***)
           (1  (test-help/- (collection op args ***)) result ***)))
    ((_ id (seq op args ***) result ***)
     (test* id (0  (test-help/- (seq op args ***)) result ***)
              (1  (test-help/- (sequence op args ***)) result ***)
              (2  (test-help/- (bag op args ***)) result ***)
              (3  (test-help/- (collection op args ***)) result ***)))))

(define-syntax testc*
  (syntax-rules (list alist-map flexible-sequence sequence)
    ((_ id (list op args ***) result ***)
     (test* id
           (0  (test-help (list op args ***)) result ***)
           (1  (test-help (flexible-sequence op args ***)) result ***)
           (2  (test-help (sequence op args ***)) result ***)
           (3  (test-help (bag op args ***)) result ***)
           (4  (test-help (collection op args ***)) result ***)))
    ((_ id (alist-map op args ***) result ***)
     (test* id
           (0  (test-help (alist-map op args ***)) result ***)
           (1  (test-help (map op args ***)) result ***)
           (1  (test-help (collection op args ***)) result ***)))
    ((_ id (seq op args ***) result ***)
     (test* id (0  (test-help (seq op args ***)) result ***)
              (1  (test-help (sequence op args ***)) result ***)
              (2  (test-help (bag op args ***)) result ***)
              (3  (test-help (collection op args ***)) result ***)))))

(define-syntax receive-first
  (syntax-rules ()
    ((_ expr ***)
     (multiple-value-call (lambda args (car args))
                          expr ***))))

;;; tests

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

(test* :Enumeration
  (0  (collection-fold-left ls1 #'ap+ 0) => 6)
  (1  (list-fold-left ls1 #'ap+ 0) => 6)
  (2  (collection-fold-right ls1 #'ap- 0) => 2)
  (3  (list-fold-right ls1 #'ap- 0) => 2)
  (4  (collection-fold-keys-left ls1 #'apcons2 '()) => ridx1)
  (5  (list-fold-keys-left ls1 #'apcons2 '()) => ridx1)
  (6  (collection-fold-keys-right ls1 #'apcons2 '()) => idx1)
  (7  (list-fold-keys-right ls1 #'apcons2 '()) => idx1)
  (8  (collection-fold-left vec1 #'ap+ 0) => 6)
  (9  (vector-fold-left vec1 #'ap+ 0) => 6)
  (10  (collection-fold-right vec1 #'ap- 0) => 2)
  (11  (vector-fold-right vec1 #'ap- 0) => 2)
  (12  (collection-fold-keys-left vec1 #'apcons2 '()) => ridx1)
  (13  (vector-fold-keys-left vec1 #'apcons2 '()) => ridx1)
  (14  (collection-fold-keys-right vec1 #'apcons2 '()) => idx1)
  (15  (vector-fold-keys-right vec1 #'apcons2 '()) => idx1)
  (16  (collection-fold-left str1 #'apsap "d") => "cbad")
  (18  (collection-fold-right str1 #'apsap "d") => "abcd")
  (20  (collection-fold-left amap1 #'ap+ 0) => 6)
  (21  (alist-map-fold-left amap1 #'ap+ 0) => 6)
  (22  (collection-fold-right amap1 #'ap+ 0) => 6)
  (23  (alist-map-fold-right amap1 #'ap+ 0) => 6)
  (24  (collection-fold-keys-left str1 #'apcons2 '()) => ridx2)
  (25  (string-fold-keys-left str1 #'apcons2 '()) => ridx2)
  (26  (collection-fold-keys-right str1 #'apcons2 '()) => idx2)
  (27  (string-fold-keys-right str1 #'apcons2 '()) => idx2)
  )

(test* :Collections-3
       (0  (collection? ls1) <> 'NIL)
       (1  (collection? ls2) <> 'NIL)
       (2  (collection? vec1) <> 'NIL)
       (3  (collection? vec2) <> 'NIL)
       (4  (collection? str1) <> 'NIL)
       (5  (collection? str2) <> 'NIL)
       (6  (collection? amap1) <> 'NIL)
       (7  (collection? amap2) <> 'NIL))

(test* :Collection-4
       (0 (collection-name ls1) => 'list)
       (1 (collection-name str1) => 'cl:string)
       (2 (collection-name vec1) => 'vector)
       (3 (collection-name amap1) => 'alist-map))

(test* :Collection-5
       (0  (list? ls1) <> 'NIL)
       (1  (string? str1) <> 'NIL)
       (2  (vector? vec1) <> 'NIL)
       (3  (alist-map? amap1) <> 'NIL))

(testc-* :Collection-6 (list size ls1) => 3)
(testc-* :Collection-7 (string size str1) => 3)
(testc-* :Collection-8 (vector size vec1) => 3)
(testc-* :Collection-9 (alist-map size amap1) => 3)

(testc-* :Collection-10 (list count ls1 1) => 1)
(testc-* :Collection-11 (string count str1 #\a) => 1)
(testc-* :Collection-12 (vector count vec1 1) => 1)
(testc-* :Collection-13 (alist-map count amap1 1) => 1)

(testc-* :Collection-14 (list count ls1 'a) => 0)
(testc-* :Collection-15 (string count str1 #\A) => 0)
(testc-* :Collection-16 (vector count vec1 'a) => 0)
(testc-* :Collection-17 (alist-map count amap1 'a) => 0)

(testc-* :Collection-18 (list get-any ls1))
(testc-* :Collection-19 (string get-any str1))
(testc-* :Collection-20 (vector get-any vec1))
(testc-* :Collection-21 (alist-map get-any amap1))

(testc-* :Collection-22 (list empty? ls1) <> 'T)
(testc-* :Collection-23 (string empty? str1) <> 'T)
(testc-* :Collection-24 (vector empty? vec1) <> 'T)
(testc-* :Collection-25 (alist-map empty? amap1) <> 'T)

(testc-* :Collection-26 (list >list ls1) => ls1)
(testc-* :Collection-27 (string >list str1) => '(#\a #\b #\c))
(testc-* :Collection-28 (vector >list vec1) => ls1)
(test*   :Collection-29
         (0  (list-contents-equal? (alist-map->list amap1) ls1) <> 'NIL)
         (1  (list-contents-equal? (map->list amap1) ls1) <> 'NIL)
         (2  (list-contents-equal? (collection->list amap1) ls1) <> 'NIL))

(testc-* :Collection-30 (list clear ls1) => (make-list))
(testc-* :Collection-31 (string clear str1) => (make-string 0))
(testc-* :Collection-32 (vector clear vec1) => (make-vector 0))

(test* :Collection-33
       (0 (map= #'equal? (alist-map-clear amap1) (make-alist-map #'equal?))
          <> 'NIL)
       (1 (map= #'equal? (map-clear amap1) (make-alist-map #'equal?))
          <> 'NIL)
       (2 (map= #'equal? (collection-clear amap1) (make-alist-map #'equal?))
          <> 'NIL))

(testc-* :Collection-34 (list clear! ls1) => (make-list))
(testc-* :Collection-35 (string clear! str1) => (make-string 0))
(testc-* :Collection-36 (vector clear! vec1) => (make-vector 0))
(test* :Collection-37
       (0 (map= #'equal? (alist-map-clear! amap1)
                (make-alist-map #'equal?) ) <> 'NIL)
       (1 (set! amap1 (alist-map '(a . 1) '(b . 2) '(c . 3))))
       (2 (map= #'equal? (map-clear! amap1)
                (make-alist-map #'equal?) ) <> 'NIL)
       (3 (set! amap1 (alist-map '(a . 1) '(b . 2) '(c . 3))))
       (4 (map= #'equal? (collection-clear! amap1)
                (make-alist-map #'equal?) ) <> 'NIL))

(test* :collection-38
      (0  (set! ls1 (list 1 2 3)))
      (1  (set! vec1 (vector 1 2 3)))
      (2  (set! str1 (string #\a #\b #\c)))
      (3  (set! amap1 (alist-map '(a . 1) '(b . 2) '(c . 3)))))

(testc* :collection-39 (list = #'equal? ls1 ls2) => 'NIL)
(testc* :collection-40 (vector = #'equal? vec1 vec2) => 'NIL)
(testc* :collection-41 (string = #'equal? str1 str2) => 'NIL)
(testc* :collection-42 (alist-map = #'equal? amap1 amap2) => 'NIL)

(test* :collection-43
       (0 (set! ls2 (collection-copy ls1)))
       (1 (set! str2 (collection-copy str1)))
       (2 (set! vec2 (collection-copy vec1)))
       (3 (set! amap2 (collection-copy amap1))))

(testc* :collection-44 (list = #'equal? ls1 ls2) <> 'NIL)
(testc* :collection-45 (vector = #'equal? vec1 vec2) <> 'NIL)
(testc* :collection-46 (string = #'equal? str1 str2) <> 'NIL)
(testc* :collection-47 (alist-map = #'equal? amap1 amap2) <> 'NIL)

(test* :collection-48
       (0 (bag? ls1) <> 'NIL)
       (1 (bag? str1) <> 'NIL)
       (2 (bag? vec1) <> 'NIL))

(test-* :collection-49 (list contains? ls1 2) <> 'NIL)
(test-* :collection-50 (string contains? str1 #\b) <> 'NIL)
(test-* :collection-51 (vector contains? vec1 2) <> 'NIL)

(test* :collection-52
       (0 (list-contents-equal? (list-add ls1 4) (list 4 1 2 3)) <> 'NIL)
       (1 (list-contents-equal? (flexible-sequence-add ls1 4) (list 4 1 2 3))
          <> 'NIL)
       (2 (list-contents-equal? (sequence-add ls1 4) (list 4 1 2 3))
          <> 'NIL)
       (3 (list-contents-equal? (bag-add ls1 4) (list 4 1 2 3))
          <> 'NIL))

(test* :collection-53
       (0 (list-contents-equal? (list-add! ls1 4) (list 4 1 2 3)) <> 'NIL)
       (1 (set! ls1 (collection-copy ls2)))
       (2 (list-contents-equal? (flexible-sequence-add! ls1 4)
                                (list 4 1 2 3) ) <> 'NIL)
       (3 (set! ls1 (collection-copy ls2)))
       (4 (list-contents-equal? (sequence-add! ls1 4) (list 4 1 2 3)) <> 'NIL)
       (5 (set! ls1 (collection-copy ls2)))
       (6 (list-contents-equal? (bag-add! ls1 4) (list 4 1 2 3)) <> 'NIL)
       (7 (set! ls1 (collection-copy ls2))) )

(test-* :collection-55 (list delete ls1 2) => '(1 3))
(test-* :collection-56 (list delete! ls1 2) => '(1 3))
(test* :collection-57 (defparameter ls1 (collection-copy ls2)))

(test-* :collection-58 (list delete-all ls1 2) => '(1 3))
(test-* :collection-59 (list delete-all! ls1 2) => '(1 3))
(test* :collection-60 (defparameter ls1 (collection-copy ls2)))
(test* :collection-61 (defparameter ls3 (list 3 4 5)))

(test* :62
      (0 (list-contents-equal? (list-add-from ls1 ls2) '(1 2 3 3 4 5)) <> 'NIL)
      (1 (list-contents-equal? (flexible-sequence-add-from ls1 ls2)
                                '(1 2 3 3 4 5)) <> 'NIL)
      (2 (list-contents-equal? (sequence-add-from ls1 ls2)
                                '(1 2 3 3 4 5)) <> 'NIL)
      (3 (list-contents-equal? (bag-add-from ls1 ls2)
                                '(1 2 3 3 4 5)) <> 'NIL))
(test* :63
      (0 (list-contents-equal? (list-add-from! ls1 ls2) '(1 2 3 3 4 5)) <> 'NIL)
      (1 (set! ls1 (collection-copy ls2)))
      (2 (list-contents-equal? (flexible-sequence-add-from! ls1 ls2)
                                '(1 2 3 3 4 5)) <> 'NIL)
      (3 (set! ls1 (collection-copy ls2)))
      (4 (list-contents-equal? (sequence-add-from! ls1 ls2)
                                '(1 2 3 3 4 5)) <> 'NIL)
      (5 (set! ls1 (collection-copy ls2)))
      (6 (list-contents-equal? (bag-add-from! ls1 ls2)
                                '(1 2 3 3 4 5)) <> 'NIL))

(test* :64
      (0 (set! ls2 (list 3)))
      (1 (list-delete-from ls1 ls2) => '(1 2))
      (2 (flexible-sequence-delete-from ls1 ls2) => '(1 2))
      (3 (sequence-delete-from ls1 ls2) => '(1 2))
      (4 (bag-delete-from ls1 ls2) => '(1 2)))

(test* :65
      (0 (list-delete-from! ls1 ls2) => '(1 2))
      (1 (set! ls2 (list 3)))
      (2 (flexible-sequence-delete-from! ls1 ls2) => '(1 2))
      (3 (set! ls2 (list 3)))
      (4 (sequence-delete-from! ls1 ls2) => '(1 2))
      (5 (set! ls2 (list 3)))
      (6 (bag-delete-from! ls1 ls2) => '(1 2)))

(test* :66 (set! ls1 (list 1 1 2 2 3 3)))
(test* :67 (set! ls3 (list 2 3)))

(test* :68
      (0 (list-delete-all-from ls1 ls3) => '(1 1))
      (1 (flexible-sequence-delete-all-from ls1 ls3) => '(1 1))
      (2 (sequence-delete-all-from ls1 ls3) => '(1 1))
      (3 (bag-delete-all-from ls1 ls3) => '(1 1)))

(test* :69
      (0 (set! ls2 (list 1 2 3)))
      (1 (set! ls1 (collection-copy ls2))))

(test* :70
      (0 (sequence? ls1) <> 'NIL)
      (1 (sequence? str1) <> 'NIL)
      (2 (sequence? vec1) <> 'NIL))

(test-* :71 sequence (list ref ls1 1) => 2)
(test-* :72 sequence (vector ref vec1 1) => 2)
(test-* :73 sequence (string ref str1 1) => #\b)

(test-* :74 sequence (list get-left ls1) => 1)
(test-* :75 sequence (vector get-left vec1) => 1)
(test-* :76 sequence (string get-left str1) => #\a)

(test-* :77 sequence (list get-right ls1) => 3)
(test-* :78 sequence (vector get-right vec1) => 3)
(test-* :79 sequence (string get-right str1) => #\c)

(test-* :80 sequence (list insert-right ls1 4) => '(1 2 3 4))

(test* :81
      (0 (list-insert-right! ls1 4) => '(1 2 3 4))
      (1 (set! ls1 (collection-copy ls2)))
      (2 (flexible-sequence-insert-right! ls1 4) => '(1 2 3 4))
      (3 (set! ls1 (collection-copy ls2)))
      (4 (sequence-insert-right! ls1 4) => '(1 2 3 4))
      (5 (set! ls1 (collection-copy ls2))))

(test-* :82 flexible-sequence (list insert-left ls1 4) => '(4 1 2 3))

(test* :83
      (0 (list-insert-left! ls1 4) => '(4 1 2 3))
      (1 (set! ls1 (collection-copy ls2)))
      (2 (flexible-sequence-insert-left! ls1 4) => '(4 1 2 3))
      (3 (set! ls1 (collection-copy ls2))))

(test-* :84 sequence (list set ls1 1 4) => '(1 4 3))

(test* :85
      (0 (list-set! ls1 1 4) => '(1 4 3))
      (1 (set! ls1 (collection-copy ls2)))
      (2 (flexible-sequence-set! ls1 1 4) => '(1 4 3))
      (3 (set! ls1 (collection-copy ls2)))
      (4 (sequence-set! ls1 1 4) => '(1 4 3))
      (5 (set! ls1 (collection-copy ls2))))

(test-* :86 sequence (list add ls1 4) => '(4 1 2 3))

(test* :87
      (0 (list-add! ls1 4) => '(4 1 2 3))
      (1 (set! ls1 (collection-copy ls2)))
      (2 (flexible-sequence-add! ls1 4) => '(4 1 2 3))
      (3 (set! ls1 (collection-copy ls2)))
      (4 (sequence-add! ls1 4) => '(4 1 2 3))
      (5 (set! ls1 (collection-copy ls2)))
      (6 (bag-add! ls1 4) => '(4 1 2 3))
      (7 (set! ls1 (collection-copy ls2))))

(test-* :88 sequence (list copy ls1) => '(1 2 3))
(test-* :89 sequence (list copy ls1 1) => '(2 3))
(test-* :90 sequence (list copy ls1 0 2) => '(1 2))
(test-* :91 sequence (list copy ls1 1 2) => '(2))

(test-* :92 flexible-sequence (list insert ls1 1 4) => '(1 4 2 3))
(test* :93
      (0 (list-insert! ls1 1 4) => '(1 4 2 3))
      (1 (set! ls1 (collection-copy ls2)))
      (2 (flexible-sequence-insert! ls1 1 4) => '(1 4 2 3))
      (3 (set! ls1 (collection-copy ls2))))

(test-* :94 flexible-sequence (list delete-at ls1 1) => '(1 3))

(test* :95
      (0 (list-delete-at! ls1 1) => '(1 3))
      (1 (set! ls1 (collection-copy ls2)))
      (2 (flexible-sequence-delete-at! ls1 1) => '(1 3))
      (3 (set! ls1 (collection-copy ls2))))

(test-* :96 flexible-sequence (list delete-left ls1) => '(2 3))

(test* :97
      (0 (list-delete-left! ls1) => '(2 3))
      (1 (set! ls1 (collection-copy ls2)))
      (2 (flexible-sequence-delete-left! ls1) => '(2 3))
      (3 (set! ls1 (collection-copy ls2))))

(test-* :98 flexible-sequence (list delete-right ls1) => '(1 2))
(test* :99
      (0 (list-delete-right! ls1) => '(1 2))
      (1 (set! ls1 (collection-copy ls2)))
      (2 (flexible-sequence-delete-right! ls1) => '(1 2))
      (3 (set! ls1 (collection-copy ls2))))

(test :100
  (is-true (map? amap1)))

(test-* :101 (alist-map equivalence-function amap1) => #'eqv?)

(test-* :102 (alist-map key-equivalence-function amap1) => #'eqv?)

(test-* :103 (alist-map contains-key? amap1 'b) <> 'NIL)

(test* :104
      (0 (list-contents-equal? (alist-map-keys->list amap1) '(a b c)) <> 'NIL)
      (1 (list-contents-equal? (map-keys->list amap1) '(a b c)) <> 'NIL))

(test-* :106 (alist-map get amap1 'b) => 2)
(test-* :107 (alist-map get amap1 'c) => 3)

(test* :108
       (0 (defparameter amap3 (alist-map '(a . 1) '(b . 2) '(c . 3) '(d . 4))))
       (1 (defparameter amap1 (alist-map '(a . 1) '(b . 2) '(c . 3))))
       (2 (set! amap2 (alist-map '(a . 1) '(b . 2) '(c . 3)))))

(test* :109
       (0 (collection= #'equal? (receive-first (alist-map-put amap1 'd 4))
                       amap3))
       (1 (collection= #'equal? (receive-first (map-put amap1 'd 4))
                       amap3)))

(test* :111
       (0 (map= #'equal? (receive-first (alist-map-put! amap1 'd 4)) amap3)
          <> 'NIL)
       (1 (set! amap1 (collection-copy amap2)))
       (2 (collection= #'equal? (receive-first (map-put! amap1 'd 4)) amap3)
          <> 'NIL)
       (3 (set! amap1 (collection-copy amap2))))

(test* :112 (set! amap3 (alist-map '(a . 1) '(b . 2) '(c . 4))))

(test :113
  (is-true
   (collection= #'equal? (receive-first (alist-map-put amap1 'c 4) amap3))))
(test :114
  (is-true
   (collection= #'equal? (receive-first (map-put amap1 'c 4) amap3))))

(test* :115
       (0 (map= #'equal? (receive-first (alist-map-put! amap1 'c 4)) amap3)
          <> 'NIL)
       (1 (set! amap1 (collection-copy amap2)))
       (2 (collection= #'equal? (receive-first (map-put! amap1 'c 4)) amap3)
          <> 'NIL)
       (3 (set! amap1 (collection-copy amap2))))

(define-function (add1 x) (+ x 1))

(test* :116 (collection= #'equal? (alist-map-update amap1 'c #'add1) amap3)
       <> 'NIL)
(test* :117 (collection= #'equal? (map-update amap1 'c #'add1) amap3)
       <> 'NIL)

(test* :118
       (0 (map= #'equal? (alist-map-update! amap1 'c #'add1) amap3) <> 'NIL)
       (1 (set! amap1 (collection-copy amap2)))
       (2 (collection= #'equal? (map-update! amap1 'c #'add1) amap3) <> 'NIL)
       (3 (set! amap1 (collection-copy amap2))))

(test* :119 (set! amap3 (alist-map '(a . 1) '(b . 2))))

(test* :120 (collection= #'equal? (alist-map-delete amap1 'c) amap3) <> 'NIL)
(test* :121 (collection= #'equal? (map-delete amap1 'c) amap3) <> 'NIL)

(test* :122
       (0 (map= #'equal? (alist-map-delete! amap1 'c) amap3) <> 'NIL)
       (1 (set! amap1 (collection-copy amap2)))
       (2 (collection= #'equal? (map-delete! amap1 'c) amap3) <> 'NIL)
       (3 (set! amap1 (collection-copy amap2))))

(test* :123 (set! amap3 (alist-map '(a . 1))))
(test* :124 (collection= #'equal? (alist-map-delete-from amap1 '(b c)) amap3)
       <> 'NIL)
(test* :125 (collection= #'equal? (map-delete-from amap1 '(b c)) amap3)
       <> 'NIL)

(test* :126
      (0 (map= #'equal? (alist-map-delete-from! amap1 '(b c)) amap3) <> 'NIL)
      (1 (set! amap1 (collection-copy amap2)))
      (2 (collection= #'equal? (map-delete-from! amap1 '(b c)) amap3) <> 'NIL)
      (3 (set! amap1 (collection-copy amap2))))

(test* :127 (set! amap3 (alist-map '(a . 1) '(b . 2) '(c . 3) '(d . 4) '(e . 5))))
(test* :128 (defparameter amap4 (alist-map '(d . 4) '(e . 5))))

(test* :129 (collection= #'equal? (alist-map-add-from amap1 amap4) amap3)
       <> 'NIL)
(test* :130 (collection= #'equal? (map-add-from amap1 amap4) amap3)
       <> 'NIL)

(test* :131
      (0 (map= #'equal? (alist-map-add-from! amap1 amap4) amap3) <> 'NIL)
      (1 (set! amap1 (collection-copy amap2)))
      (2 (collection= #'equal? (map-add-from! amap1 amap4) amap3) <> 'NIL)
      (3 (set! amap1 (alist-map))))

(testc-* :132 (list get-any '() (lambda () 'false)) => 'false)
(testc-* :133 (vector get-any '#() (lambda () 'false)) => 'false)
(testc-* :134 (string get-any "" (lambda () 'false)) => 'false)
(testc-* :135 (alist-map get-any amap1 (lambda () 'false)) => 'false)

(test-* :136 sequence (list get-left '() (lambda () 'false)) => 'false)
(test-* :137 sequence (vector get-left '#() (lambda () 'false)) => 'false)
(test-* :138 sequence (string get-left "" (lambda () 'false)) => 'false)

(test-* :139 sequence (list get-right '() (lambda () 'false)) => 'false)
(test-* :140 sequence (vector get-right '#() (lambda () 'false)) => 'false)
(test-* :141 sequence (string get-right "" (lambda () 'false)) => 'false)

(test-* :141 sequence (list ref '() 0 (lambda () 'false)) => 'false)
(test-* :142 sequence (vector ref '#() 0 (lambda () 'false)) => 'false)
(test-* :143 sequence (string ref "" 0 (lambda () 'false)) => 'false)

(test-* :144 (alist-map get amap1 'a (lambda () 'false)) => 'false)

(test* :145
      (0 (call-with-values (lambda ()
                               (alist-map-put amap1 'a 4 (lambda () 'false)))
             (lambda (map val) map val)) => 'false)
      (1 (multiple-value-call (lambda (map val) map val)
                              (map-put amap1 'a 4 (lambda () 'false)))
         => 'false))

(test* :146
      (0 (call-with-values (lambda ()
                              (alist-map-put! amap1 'a 4 (lambda () 'false)))
            (lambda (map val) map val)) => 'false)
      (1 (set! amap1 (alist-map)))
      (2 (multiple-value-call (lambda (map val) map val)
                              (map-put! amap1 'a 4 (lambda () 'false)))
         => 'false)
      (3 (set! amap1 (alist-map))))

(test* :147
      (0 (alist-map-get
          (alist-map-update amap1 'a #'add1 (lambda () 1)) 'a) => 2)
      (1 (alist-map-get
          (map-update amap1 'a #'add1 (lambda () 1)) 'a) => 2))

(test* :148
      (0 (alist-map-get
          (alist-map-update! amap1 'a #'add1 (lambda () 1)) 'a) => 2)
      (1 (set! amap1 (alist-map)))
      (2 (alist-map-get
            (map-update! amap1 'a #'add1 (lambda () 1)) 'a) => 2)
      (3 (set! amap1 (alist-map))))

;;; eof
