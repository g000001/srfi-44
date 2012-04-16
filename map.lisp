(cl:in-package :srfi-44.internal)

(defclass <map> (<collection>) ())
(defvar <map> (find-class '<map>))

;;; - Maps -

(defun map? (obj)
  (typep obj <map>))

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

(define-function (map= elt=? . maps)
  (for-each (lambda (map) (check-arg #'map? map 'map=)) maps)
  (apply #'collection= elt=? maps))

(defgeneric map-equivalence-function (map))
(defgeneric map-key-equivalence-function (map))
(defgeneric map-contains-key? (map key))
(defgeneric map-keys->list (map))
(defgeneric map-get (map key &optional absence-thunk))
(defgeneric map-put (map key value &optional absence-thunk))
(defgeneric map-put! (map key value &optional absence-thunk))
(defgeneric map-update (map key func &optional absence-thunk))
(defgeneric map-update! (map key func &optional absence-thunk))
(defgeneric map-delete (map key))
(defgeneric map-delete! (map key))
(defgeneric map-delete-from (map bag))
(defgeneric map-delete-from! (map bag))
(defgeneric map-add-from (map source-map))
(defgeneric map-add-from! (map source-map))

;;; eof
