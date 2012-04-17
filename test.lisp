(cl:in-package :srfi-44.internal)

(define-syntax test*
  (syntax-rules (=> <>)
    ((test* name
            (num expr => result) ***)
     (test name
       (is (equal result expr)) ***))
    ((test* name
            (num expr <> result) ***)
     (test name
       (is (not (equal result expr))) ***))))

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

;;; eof
