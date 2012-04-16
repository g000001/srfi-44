;; Copyright (C) 2003 Taylor Campbell and Scott G. Miller.  See the
;; LICENCE file for details.

;; This reference implementation uses Gregor Kiczales' Tiny-CLOS.

;; This file requires utilities.scm, utilities_tiny-clos.scm, SRFI 2
;; (and-let*), SRFI 23 (error), and Tiny-CLOS.

(cl:in-package :srfi-44.internal)

;; Attributes
(defclass <top> () ())
(defclass <attribute> (<top>) ())
(defvar <attribute> (find-class '<attribute>))
(defclass <ordered-attribute> (<attribute>) ())
(defvar <ordered-attribute> (find-class '<ordered-attribute>))
(defclass <directional-attribute> (<attribute>) ())
(defvar <directional-attribute> (find-class '<directional-attribute>))
(defclass <purely-mutable-attribute> (<attribute>) ())
(defvar <purely-mutable-attribute> (find-class '<purely-mutable-attribute>))
(defclass <limited-attribute> (<attribute>) ())
(defvar <limited-attribute> (find-class '<limited-attribute>))

(defgeneric collection-get-any (coll &optional maybe-ft))

;;; - Attributes -

(defun ordered-collection? (obj)
  (typep obj <ordered-attribute>))

(defgeneric collection-ordering-function ())
(defgeneric collection-get-left ())
(defgeneric collection-get-right ())
(defgeneric collection-delete-left ())
(defgeneric collection-delete-left! ())
(defgeneric collection-delete-right ())
(defgeneric collection-delete-right! ())

(defun directional-collection? (obj)
  (typep obj <directional-attribute>))


(defgeneric collection-insert-left ())
(defgeneric collection-insert-left! ())
(defgeneric collection-insert-right ())
(defgeneric collection-insert-right! ())

(defun purely-mutable-collection? (obj)
  (typep obj <purely-mutable-attribute>))

(defun limited-collection? (obj)
  (typep obj <limited-attribute>))

;;;;;; - Scheme Collections -

;; Auxiliaries

;;;;;; - Scheme Collections -

#|(add-methods
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
       (list <list>)))|#

#|(add-methods
 (list #'map-equivalence-function     #'alist-map-equivalence-function     (list <alist-map>))
 (list #'map-key-equivalence-function #'alist-map-key-equivalence-function (list <alist-map>))
 (list #'map-contains-key? #'alist-map-contains-key? (list <alist-map>))
 (list #'map-keys->list #'alist-map-keys->list (list <alist-map>))
 (list #'map-get #'alist-map-get (list <alist-map>))
 (list #'map-put  #'alist-map-put  (list <alist-map>))
 (list #'map-put! #'alist-map-put! (list <alist-map>))
 (list #'map-update  #'alist-map-update  (list <alist-map>))
 (list #'map-update! #'alist-map-update! (list <alist-map>))
 (list #'map-delete  #'alist-map-delete  (list <alist-map>))
 (list #'map-delete! #'alist-map-delete! (list <alist-map>))
 (list #'map-delete-from  #'alist-map-delete-from  (list <alist-map>))
 (list #'map-delete-from! #'alist-map-delete-from! (list <alist-map>))
 (list #'map-add-from  #'alist-map-add-from  (list <alist-map>))
 (list #'map-add-from! #'alist-map-add-from! (list <alist-map>)))|#


(defmethod collection-fold-keys-left (coll fold-function &rest seed)
  (apply (etypecase coll
           (string #'string-fold-keys-left)
           (vector #'vector-fold-keys-left)
           (list #'list-fold-keys-left)
           (<alist-map> #'alist-map-fold-keys-left) )
         coll
         fold-function
         seed ))

(defmethod collection-fold-keys-right (coll fold-function &rest seed)
  (apply (etypecase coll
             (string #'string-fold-keys-right)
             (vector #'vector-fold-keys-right)
             (list #'list-fold-keys-right)
             (<alist-map> #'alist-map-fold-keys-right))
         coll
         fold-function
         seed))

(defmethod collection-fold-left (coll fold-function &rest seed)
  (apply (etypecase coll
             (string #'string-fold-left)
             (vector #'vector-fold-left)
             (list #'list-fold-left)
             (<alist-map> #'alist-map-fold-left))
         coll
         fold-function
         seed))

(defmethod collection-fold-right (coll fold-function &rest seed)
  (apply (etypecase coll
             (string #'string-fold-right)
             (vector #'vector-fold-right)
             (list #'list-fold-right)
             (<alist-map> #'alist-map-fold-right))
         coll
         fold-function
         seed))

(defmethod collection-size (coll)
  (funcall (etypecase coll
             (string #'string-size)
             (vector #'vector-size)
             (list #'list-size)
             (<alist-map> #'alist-map-size))
           coll ))

(defmethod collection-count (coll value)
  (funcall (etypecase coll
             (string #'string-count)
             (vector #'vector-count)
             (list #'list-count)
             (<alist-map> #'alist-map-count))
           coll
           value ))

(defmethod collection-empty? (coll)
  (funcall (etypecase coll
             (string #'string-empty?)
             (vector #'vector-empty?)
             (list #'list-empty?)
             (<alist-map> #'alist-map-empty?))
           coll ))

(defmethod collection-get-any (coll &optional maybe-ft)
  (apply (etypecase coll
           (string #'string-get-any)
           (vector #'vector-get-any)
           (list #'list-get-any)
           (<alist-map> #'alist-map-get-any))
         coll
         (and maybe-ft (list maybe-ft))))

(defmethod collection-copy (coll)
  (funcall (etypecase coll
             (string #'string-copy)
             (vector #'vector-copy)
             (list #'list-copy)
             (<alist-map> #'alist-map-clear))
           coll ))

(defmethod collection-clear (coll)
  (funcall (etypecase coll
             (string #'string-clear)
             (vector #'vector-clear)
             (list #'list-clear)
             (<alist-map> #'alist-map-clear))
           coll ))

(defmethod collection-clear! (coll)
  (funcall (etypecase coll
             (string #'string-clear!)
             (vector #'vector-clear!)
             (list #'list-clear!)
             (<alist-map> #'alist-map-clear!))
           coll ))

(defmethod collection=-2op (pred col1 col2)
  (funcall (etypecase col1
             (string #'string=)
             (vector #'vector=)
             (list #'list=) )
           pred col1 col2))

(defmethod bag-equivalence-function (object)
  (funcall (etypecase object
             (string #'string-equivalence-function)
             (vector #'vector-equivalence-function)
             (list #'list-equivalence-function) )
           object))

(defmethod bag-contains? (bag value)
  (funcall (etypecase bag
             (string #'string-contains?)
             (vector #'vector-contains?)
             (list #'list-contains?) )
           bag
           value))

(defmethod sequence-ref (sequence integer &optional absence-thunk)
  (apply (etypecase sequence
           (string #'string-ref)
           (vector #'vector-ref)
           (list #'list-ref) )
         sequence
         integer
         (and absence-thunk (list absence-thunk)) ))

(defmethod sequence-set (sequence integer value)
  (funcall (etypecase sequence
             (string #'string-set)
             (vector #'vector-set)
             (list #'list-set) )
           sequence
           integer
           value))

(defmethod sequence-set! (sequence integer value)
  (funcall (etypecase sequence
             (string #'string-set!)
             (vector #'vector-set!)
             (list #'list-set!) )
           sequence
           integer
           value))

(defmethod sequence-get-left (sequence &optional absence-thunk)
  (apply (etypecase sequence
           (string #'string-get-left)
           (vector #'vector-get-left)
           (list #'list-get-left) )
         sequence
         (and absence-thunk (list absence-thunk)) ))

(defmethod sequence-get-right (sequence &optional absence-thunk)
  (apply (etypecase sequence
           (string #'string-get-right)
           (vector #'vector-get-right)
           (list #'list-get-right))
         sequence
         (and absence-thunk (list absence-thunk))))

(defmethod sequence-replace-from! ((sequence cl:sequence)
                                  dest-start source-sequence
                                  &optional source-start source-end)
  (apply (etypecase sequence
           (string #'string-replace-from!)
           (vector #'vector-replace-from!)
           (list #'list-replace-from!))
         sequence
         dest-start
         source-sequence
         `(,@(and source-start (list source-start))
             ,@(and source-end (list source-end) ))))

(defmethod sequence-replace-from ((sequence cl:sequence)
                                  dest-start source-sequence
                                  &optional source-start source-end)
  (apply (etypecase sequence
           (string #'string-replace-from)
           (vector #'vector-replace-from)
           (list #'list-replace-from))
         sequence
         dest-start
         source-sequence
         `(,@(and source-start (list source-start))
             ,@(and source-end (list source-end) ))))

(defmethod sequence-copy ((sequence cl:sequence) &optional (start 0) end)
  (apply (etypecase sequence
           (string #'string-copy)
           (vector #'vector-copy)
           (list #'list-copy))
         sequence
         start
         (and end (list end))))
