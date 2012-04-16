(cl:in-package :srfi-44.internal)

;; LIST is defined by R5RS.
; (define (list . l) l)

#|(add-method #'collection-name
  (method next-method ((l <list>)) 'list))|#

;;; - Lists -

(define-function (make-list . maybe-size+fill)
  (let*-optionals #'make-list maybe-size+fill
      ((size 0) (fill :undef))
    (srfi-5:let loop ((l '()) (i 0))
      (if (= i size)
          l
          (loop (cons fill l) (+ i 1))))))

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

(define-function (list-equivalence-function l)
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

;;; eof
