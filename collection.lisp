(cl:in-package :srfi-44.internal)

(defmacro define-collection-method (collection-foo-name (&rest args)
                                    &rest classes)
  (let* ((name (cl:string collection-foo-name))
         (suf (intern (subseq name (mismatch "COLLECTION" name)))))
    `(progn
       ,@(mapcar (lambda (c)
                   `(defmethod ,collection-foo-name ((coll ,c) ,@args)
                      (,(intern (format nil
                                        "~A~A"
                                        (string-trim "<>" c)
                                        suf
                                        ))
                        coll ,@args)))
                 classes))))

;;; FIXME
(defmacro define-collection-method* (collection-foo-name (&rest args)
                                     &rest classes)
  (let* ((name (cl:string collection-foo-name))
         (suf (intern (subseq name (mismatch "COLLECTION" name)))))
    `(progn
       ,@(mapcar (lambda (c)
                   `(defmethod ,collection-foo-name ((coll ,c) ,@args)
                      (apply #',(intern (format nil
                                                "~A~A"
                                                (string-trim "<>" c)
                                                suf
                                                ))
                             coll ,@(cl:remove '&rest args))))
                 classes))))

(defclass <collection> ()
  ((contents :initarg :contents :accessor contents)
   (equivalence-function :accessor collection-equivalence-function
               :initarg :equivalence-function
               :initform #'eql)))

(defvar <collection> (find-class '<collection>))

;;; procedure: collection? value => value
;;; procedure: %? value => value
(defgeneric collection? (obj)
  (:documentation
   "Returns a non-false value if the provided value is a collection."))

(defmethod collection? ((obj <collection>))
  T)

(defmethod collection? ((obj cl:sequence)) T)
(defmethod collection? ((obj <alist-map>)) T)

;;; procedure: collection-name collection => symbol (%)
(defgeneric collection-name (coll)
  (:documentation "Returns the collection name of the provided collection. The name is a symbol containing the type name of the specific collection. A collection whose constructor is make-list, for example, would have the symbol list returned from collection-name"))

(defmethod collection-name ((coll <collection>))
  (class-name (class-of coll)))
;; (defmethod collection-name ((l <list>)) 'list)
(defmethod collection-name ((l list)) 'list)

;;; procedure: *-size * => exact integer
(defgeneric collection-size (coll)
  (:documentation "If the collection has a concept of size, this function returns the number of actual values in the collection (or mapped values in a map) If it does not, #f must be returned. When an integer is returned from this function, it indicates that an enumeration will proceed exactly *-size steps in the absence of any updates. "))

(defmethod collection-size ((coll <collection>))
  (length (contents coll)))

#|(defmethod collection-size ((coll sequence))
  (collection-fold-left coll
                        ;; (lambda (x s)
                        (lambda (s)
                          ;; (declare (ignore x))
                          (values 'T (+ s 1)))
                        0))|#

(define-collection-method collection-size ()
  list vector cl:string <alist-map>)

;;; procedure: *-count * value=> exact integer
(defgeneric collection-count (coll value)
  (:documentation "Counts the number of times value occurs in the collection, according to the collection's equivalence function." ))

(defmethod collection-count ((coll <collection>) value)
  (cl:count value (contents coll)))

(define-collection-method collection-count (elt)
  list vector cl:string <alist-map>)

;;; procedure: *-get-any * [absence-thunk]=> value
(defgeneric collection-get-any (coll &optional absence-thunk)
  (:documentation "Returns a value from the given collection. It is unspecified which of its values is returned. If the collection is empty, absence-thunk is invoked if present and its value returned, otherwise #f is returned."))

(defmethod collection-get-any ((coll <collection>)
                               &optional (absence-thunk (constantly 'NIL)))
  (or (car (contents coll))
      (funcall absence-thunk)))

(defmethod collection-get-any ((coll <map>)
                               &optional (absence-thunk (constantly 'NIL)))
  (or (car (cdr (contents coll)))
      (funcall absence-thunk)))

;;; procedure: *-empty? * => boolean
(defgeneric collection-empty? (coll)
  (:documentation "Returns a non-false value iff the given collection is known to be empty. This function should return false if it is known that there are values within the collection, or if it is unknown whether any values exist."))

(defmethod collection-empty? ((coll <collection>))
  (zerop (collection-size coll)))

(define-collection-method collection-empty? ()
  list vector cl:string <alist-map>)

;;; procedure: *->list * => list
(defgeneric collection->list (coll)
  (:documentation "Returns a newly allocated list containing the values of the collection. This can be done trivially with enumeration, but an implementation may choose to allow this function to behave more efficiently on certain collections. If the collection is ordered, the list must contain the values of the collection in the same order as the left collection fold."))

(defmethod collection->list ((coll <collection>))
  (contents coll))

(defmethod collection->list ((coll <alist-map>))
  (alist-map->list coll))

(defmethod collection->list ((coll cl:sequence))
  (coerce coll 'list))

;;; procedure: *-clear * => %
;;; procedure: *-clear! * => %
(defgeneric collection-clear! (coll)
  (:documentation "Clears the collection. In all cases a collection of the same type as the input is returned with no values or mappings. It is an error to clear an immutable collection and may be an error to clear a limited collection."))

(defmethod collection-clear! ((coll <collection>))
  (setf (contents coll) '() )
  coll)

(defmethod collection-clear! ((coll <map>))
  (setf (contents coll) (cons (list 'unique) nil))
  coll)

(defgeneric collection-clear (coll)
  (:documentation "Clears the collection. In all cases a collection of the same type as the input is returned with no values or mappings. It is an error to clear an immutable collection and may be an error to clear a limited collection."))

(defmethod collection-clear ((coll <collection>))
  (collection-clear! (collection-copy coll)))

(defmethod collection=-2op ((pred function) (col1 <collection>) (col2 <collection>))
  (let ((c1 (cl:remove-duplicates (contents col1)))
        (c2 (cl:remove-duplicates (contents col2))))
    (and (= (length c1) (length c2))
         (null (cl:set-difference (cl:union c1 c2 :test pred)
                                  c1
                                  :test pred)))))

;;; procedure: *= elt= * ... => boolean
(defun collection= (pred &rest colls)
  "Compares the provided (zero or more) collections for equivalence given the equivalence predicate elt=. If fewer than two collections are provided, a non-false value is returned. For any other number of collections, the collections are compared pairwise from left to right. As soon as any two collections contain different numbers of values or mappings, or their values aren't equivalent as in the section \"Equivalence\" above with the provided elt= function used for value comparison, false is returned. If all collections are equivalent, a non-false value is returned."
  (or (null colls)
      (cl:every (lambda (x)
                  (collection=-2op pred (car colls) x))
                (cdr colls))))

#||

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
||#


;;; procedure: make-% => %
(defgeneric make-collection ()
  (:documentation "Constructs a % collection."))

(defmethod make-collection ()
  (make-instance <collection> :contents '()))

;;; procedure: % value ... => %
(defgeneric collection (&rest args)
  (:documentation "Constructs a % collection with zero or more values provided as its initial contents."))

(defmethod collection (&rest args)
  (let ((coll (make-collection)))
    (setf (contents coll) args)
    coll))

;;; procedure: *-copy * => %
(defgeneric collection-copy (coll)
  (:documentation "Creates a new collection whose type and contents are the same as the collection passed as an operand, but which is distinct enough in storage that the new collection cannot be affected by modifications to the input collection and vice versa. This copy is shallow, that is, values are copied to the new collection in a way that preserves object identity."))

(defmethod collection-copy ((coll <collection>))
  (make-instance (class-of coll)
                 :contents (copy-list (contents coll))))

(defmethod collection-copy ((coll <map>))
  (make-instance (class-of coll)
                 :contents (copy-alist (contents coll))))

;;;; Enumeration

;;; procedure: collection-fold-left collection fold-function seed ... => seed ...
;;; procedure: %-fold-left % fold-function seed ... => seed ...
(defgeneric collection-fold-left (collection fold-function &rest seed)
  (:documentation "fold-function is a procedure which accepts one more than the number of seed values. The function accepts a single collection value as its first argument, and the seeds as remaining arguments. It must then return a proceed value, which if false halts the enumeration, as well as an equal number of returned seed values as arguments. These seed values are then passed to the next call to the fold function on the next collection value.
When the collection values are exhausted or a false proceed value is received from the fold function, the enumeration ceases and the fold operator returns the last set of seed values returned by the fold-function."))

(defmethod collection-fold-left ((coll <collection>) f &rest seeds)
  (apply #'list-fold-left (contents coll) f seeds))

#|(defmethod collection-fold-left ((seq sequence) f &rest seeds)
  (let ((size (sequence-size seq))
        (seed-count (list-size seeds)) )
    (srfi-5:let loop ((seeds seeds) (i 0))
                (print (list seeds i))
                (if (>= i size)
                    (apply #'values seeds)
                    (receive (proceed? . new-seeds)
                             (apply f seeds)
                      (if (= (list-size new-seeds) seed-count)
                          (if proceed?
                              (loop new-seeds (+ i 1))
                              (apply #'values new-seeds) )
                          (error "(sequence)-fold-left: Wrong seed count"
                                 `(expected ,seed-count)
                                 `(got ,(list-size new-seeds)) )))))))|#

(define-collection-method* collection-fold-left (f &rest seeds)
  list vector cl:string <alist-map>)

;;; procedure: collection-fold-right collection fold-function seed ... => seed ...
;;; procedure: %-fold-right % fold-function seed ... => seed ...
(defgeneric collection-fold-right (collection fold-function &rest seed)
  (:documentation "Behaves like collection-fold-left, except that the fold function is applied to the values of the collection in reverse order."))

;;; procedure: collection-fold-keys-left collection fold-function seed ... => seed ...
;;; procedure: %-fold-keys-left % fold-function seed ... => seed ...
(defgeneric collection-fold-keys-left (collection fold-function &rest seed)
  (:documentation "Behaves like collection-fold-left, but enumerates over the keys or indices of a map or sequence respectively as well as the corresponding values. This procedure is only defined for sequences and maps.
Also, the fold function of a key enumerator must accept two more operands than the number of seed values. The first two operands to the function are the key and corresponding value at the current point in the enumeration."))

;;; procedure: collection-fold-keys-right collection fold-function seed ... => seed ...
;;; procedure: %-fold-keys-right % fold-function seed ... => seed ...
(defgeneric collection-fold-keys-right (collection fold-function &rest seed)
  (:documentation "Behaves like collection-fold-keys-left, but enumerates in the reverse order over the keys or indices of an ordered map or sequence and values. This procedure is only defined for sequences and ordered maps."))

;;; eof
