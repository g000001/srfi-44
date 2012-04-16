(cl:in-package :srfi-44.internal)

(defclass <set> (<collection>)
  ((equivalence-function :accessor set-equivalence-function)))

(defvar <set> (find-class  '<set>))

;;; - Sets -

(defun set? (obj)
  (typep obj <set>))

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

(defmethod set-contains? ((set <set>) value)
  (find value (contents set) :test (set-equivalence-function set)))

(defmethod set-subset ((set <set>) value)
  (find value (contents set) :test (set-equivalence-function set)))

(defgeneric set-subset? (set &rest sets))
(defgeneric set-add (set value))
(defgeneric set-add! (set value))
(defgeneric set-delete (set value))
(defgeneric set-delete! (set value))
(defgeneric set-union (set &rest sets))
(defgeneric set-union! (set &rest sets))
(defgeneric set-intersection (set &rest sets))
(defgeneric set-intersection! (set &rest sets))
(defgeneric set-difference (set &rest sets))
(defgeneric set-difference! (set &rest sets))
(defgeneric set-symmetric-difference (set1 set2))
(defgeneric set-symmetric-difference! (set1 set2))
(defgeneric set-add-from (set bag))
(defgeneric set-add-from! (set bag))
(defgeneric set-delete-from (set bag))
(defgeneric set-delete-from! (set bag))

(defmethod collection-count ((set <set>) elt)
  (if (collection-count set elt) 1 0))

;;; eof
