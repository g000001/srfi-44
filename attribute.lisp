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

(define-function collection-get-any #'values) ;dummy

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

 (list #'bag-equivalence-function #'list-equivalence-function (list <list>))

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
       (list <list>)))|#

#|(add-methods
 (list #'collection-fold-left  #'alist-map-fold-left  (list <alist-map>))
 (list #'collection-fold-right #'alist-map-fold-right (list <alist-map>))

 (list #'collection-fold-keys-left  #'alist-map-fold-keys-left
       (list <alist-map>))
 (list #'collection-fold-keys-right #'alist-map-fold-keys-right
       (list <alist-map>))

 (list #'collection-size #'alist-map-size (list <alist-map>))
 (list #'collection-count #'alist-map-count (list <alist-map>))

 (list #'map-equivalence-function     #'alist-map-equivalence-function     (list <alist-map>))
 (list #'map-key-equivalence-function #'alist-map-key-equivalence-function (list <alist-map>))

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
 (list #'map-add-from! #'alist-map-add-from! (list <alist-map>)))|#

#|(add-methods
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

 (list #'bag-equivalence-function #'vector-equivalence-function (list <vector>))

 (list #'bag-contains? #'vector-contains? (list <vector>))

 ;;;

 (list #'sequence-ref  #'vector-ref  (list <vector> <number>))
 (list #'sequence-set  #'vector-set  (list <vector> <number>))
 (list #'sequence-set! #'vector-set! (list <vector> <number>))

 (list #'sequence-get-left  #'vector-get-left  (list <vector>))
 (list #'sequence-get-right #'vector-get-right (list <vector>)) )|#

#|(add-methods
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

 (list #'bag-equivalence-function #'string-equivalence-function (list <string>))

 (list #'bag-contains? #'string-contains? (list <string>))

 ;;;
 (list #'sequence-ref  #'string-ref  (list <string> <number>))
 (list #'sequence-set  #'string-set  (list <string> <number>))
 (list #'sequence-set! #'string-set! (list <string> <number>))

 (list #'sequence-get-left  #'string-get-left  (list <string>))
 (list #'sequence-get-right #'string-get-right (list <string>))
 )|#

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
