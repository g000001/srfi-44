(cl:in-package :srfi-44.internal)

;;; - Alists -

(defmethod collection-name ((lmap <alist-map>))
  'alist-map)

(defclass <alist-map> (<map>)
  ((equivalence-function :initarg :equivalence-function)
   ;; contents: format (:dummy elt ...) tconc?
   ))

(defvar <alist-map> (find-class '<alist-map>))

(defun alist-map? (obj)
  (typep obj <alist-map>))

(defun make<alist-map> (ignore equivalence-function contents)
  (declare (ignore ignore))
  (make-instance <alist-map>
                 :equivalence-function equivalence-function
                 :contents (cons (list 'unique) contents)))


(define-function (alist-map= elt=? . maps)
  (for-each (lambda (map) (check-arg #'alist-map? map 'alist-map=)) maps)
  (apply #'map= elt=? maps))

(define-function (make-alist-map . maybe-key-equivalence-function)
  (let*-optionals #'make-alist-map maybe-key-equivalence-function
                  ((key-equivalence-function #'eqv?))
    (make<alist-map> <alist-map> key-equivalence-function '())))
(define-function (copy-pair pair)
  (cons (car pair) (cdr pair)))
(define-function (alist-map . args)
  (cond ((null? args)
         (make<alist-map> <alist-map> #'eqv? '()))
        ((procedure? (car args))
         (make<alist-map> <alist-map> (car args) (map #'copy-pair (cdr args))))
        (:else
         (make<alist-map> <alist-map> #'eqv? (map #'copy-pair args)))))

#|(define-function alist-map? (make-generic-predicate <alist-map>))|#

#|(add-method #'collection-name
  (method next-method ((l <alist-map>)) 'alist-map))|#

(define-syntax destructure-alist-map
  (syntax-rules ()
    ((destructure-alist-map ?lmap ('NIL 'NIL 'NIL)
       ?body1 ?body2 ***)
     (let () ?body1 ?body2 ***))
    ((destructure-alist-map ?lmap (?compare 'NIL 'NIL)
       ?body1 ?body2 ***)
     (let ((?compare (slot-value ?lmap 'equivalence-function)))
       ?body1 ?body2 ***))
    ((destructure-alist-map ?lmap ('NIL ?contents 'NIL)
       ?body1 ?body2 ***)
     (let ((?contents (slot-value ?lmap 'contents)))
       ?body1 ?body2 ***))
    ((destructure-alist-map ?lmap (?compare ?contents 'NIL)
       ?body1 ?body2 ***)
     (let ((?compare (slot-value ?lmap 'equivalence-function))
           (?contents (slot-value ?lmap 'contents)))
       ?body1 ?body2 ***))
    ((destructure-alist-map ?lmap ('NIL 'NIL ?contents-cdr)
       ?body1 ?body2 ***)
     (let ((?contents-cdr (cdr (slot-value ?lmap 'contents))))
       ?body1 ?body2 ***))
    ((destructure-alist-map ?lmap (?compare 'NIL ?contents-cdr)
       ?body1 ?body2 ***)
     (let ((?compare (slot-value ?lmap 'equivalence-function))
           (?contents-cdr (cdr (slot-value ?lmap 'contents))))
       ?body1 ?body2 ***))
    ((destructure-alist-map ?lmap ('NIL ?contents ?contents-cdr)
       ?body1 ?body2 ***)
     (let ((?contents (slot-value ?lmap 'contents))
           (?contents-cdr (cdr (slot-value ?lmap 'contents))))
       ?body1 ?body2 ***))
    ((destructure-alist-map ?lmap (?compare ?contents ?contents-cdr)
       ?body1 ?body2 ***)
     (let ((?compare (slot-value ?lmap 'equivalence-function))
           (?contents (slot-value ?lmap 'contents))
           (?contents-cdr (cdr (slot-value ?lmap 'contents))))
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

(define-function (alist-map-key-equivalence-function lmap)
  (slot-value lmap 'equivalence-function))
(define-function alist-map-equivalence-function #'alist-map-key-equivalence-function)

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

#|(add-method #'collection-get-any
  (method next-method ((map <map>) . maybe-ft)
    (declare (ignore next-method))
    (apply #'alist-map-get-any map maybe-ft)))|#

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
  (length (cdr (slot-value lmap 'contents))))

(define-function (alist-map-empty? lmap)
  (null? (cdr (slot-value lmap 'contents))))

(define-function (alist-map-copy lmap)
  (destructure-alist-map lmap (compare 'NIL contents)
    (make<alist-map> <alist-map> compare
          ;Must use a deep copy here
          (map #'copy-pair contents))))
(define-function (alist-map->list lmap)
  (map #'cdr (cdr (slot-value lmap 'contents))))
(define-function (alist-map-keys->list lmap)
  (map #'car (cdr (slot-value lmap 'contents))))

(define-function (alist-map-clear lmap)
  (make<alist-map> <alist-map> (slot-value lmap 'equivalence-function) '()))
(define-function (alist-map-clear! lmap)
  (set-cdr! (slot-value lmap 'contents) '())
  ;(slot-set! lmap 'contents (list (list 'unique)))
  lmap)

#|(define-function alist-map? (make-generic-predicate <alist-map>))|#

(define-function (alist-map-get lmap key . maybe-ft)
  (let*-optionals #'alist-map-get maybe-ft
      ((ft (lambda () 'NIL)))
    #|(LET ((COMPARE (SLOT-VALUE LMAP 'EQUIVALENCE-FUNCTION))
          (CONTENTS (CDR (print (SLOT-VALUE LMAP 'CONTENTS)))))
      (ITERATE LOOP
        ((L CONTENTS))
        (COND ((NULL? L) (FUNCALL FT)) ((FUNCALL COMPARE (CAAR L) KEY) (CDAR L))
              (:ELSE (LOOP (CDR L))))))|#
    (destructure-alist-map lmap (compare  'NIL contents)
      (iterate loop ((l contents))
        (cond ((null? l)
               (funcall ft))
              ((funcall compare (caar l) key)
               (cdar l))
              (:else
               (loop (cdr l))))))
    #|(destructure-alist-map lmap (compare  'NIL contents)
      (iterate loop ((l contents))
        (cond ((null? l)
               (funcall ft))
              ((funcall compare (caar l) key)
               (cdar l))
              (:else
               (loop (cdr l))))))|#))

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

        (values (make<alist-map> <alist-map> compare contents) prv)))))

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
      (make<alist-map> <alist-map> compare
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
    (make<alist-map> <alist-map> compare
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
