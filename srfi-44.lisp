;;;; srfi-44.lisp

(cl:in-package :srfi-44.internal)

(def-suite srfi-44)

(in-suite srfi-44)

(defmethod bag-add (bag value)
  (funcall (etypecase bag
             (list #'list-add))
           bag
           value))

(defmethod bag-add! (bag value)
  (funcall (etypecase bag
             (list #'list-add!))
           bag
           value))

(defmethod bag-delete (bag value)
  (funcall (etypecase bag
             (list #'list-delete))
           bag
           value))

(defmethod bag-delete! (bag value)
  (funcall (etypecase bag
             (list #'list-delete!))
           bag
           value))

(defmethod bag-delete-all (bag value)
  (funcall (etypecase bag
             (list #'list-delete-all))
           bag
           value))

(defmethod bag-delete-all! (bag value)
  (funcall (etypecase bag
             (list #'list-delete-all!))
           bag
           value))

(defmethod bag-add-from (bag source-bag)
  (funcall (etypecase bag
             (list #'list-add-from))
           bag
           source-bag))

(defmethod bag-add-from! (bag source-bag)
  (funcall (etypecase bag
             (list #'list-add-from!))
           bag
           source-bag))

(defmethod bag-delete-from (bag source-bag)
  (funcall (etypecase bag
             (list #'list-delete-from))
           bag
           source-bag))

(defmethod bag-delete-from! (bag source-bag)
  (funcall (etypecase bag
             (list #'list-delete-from!))
           bag
           source-bag))

(defmethod bag-delete-all-from (bag source-bag)
  (funcall (etypecase bag
             (list #'list-delete-all-from))
           bag
           source-bag))

(defmethod bag-delete-all-from! (bag source-bag)
  (funcall (etypecase bag
             (list #'list-delete-all-from!))
           bag
           source-bag))

(defmethod flexible-sequence-insert (fseq index value)
  (funcall (etypecase fseq
             (list #'list-insert))
           fseq
           index
           value))

(defmethod flexible-sequence-insert! (fseq index value)
  (funcall (etypecase fseq
             (list #'list-insert!))
           fseq
           index
           value))

(defmethod flexible-sequence-delete-at (fseq index)
  (funcall (etypecase fseq
             (list #'list-delete-at))
           fseq
           index))

(defmethod flexible-sequence-delete-at! (fseq index)
  (funcall (etypecase fseq
             (list #'list-delete-at!))
           fseq
           index))

(defmethod sequence-insert-left (fseq value)
  (funcall (etypecase fseq
             (list #'list-insert-left))
           fseq
           value))

(defmethod sequence-insert-left! (fseq value)
  (funcall (etypecase fseq
             (list #'list-insert-left!))
           fseq
           value))

(defmethod sequence-insert-right (fseq value)
  (funcall (etypecase fseq
             (list #'list-insert-right))
           fseq
           value))

(defmethod sequence-insert-right! (fseq value)
  (funcall (etypecase fseq
             (list #'list-insert-right!))
           fseq
           value))

(defmethod flexible-sequence-delete-left (fseq)
  (funcall (etypecase fseq
             (list #'list-delete-left))
           fseq))

(defmethod flexible-sequence-delete-left! (fseq)
  (funcall (etypecase fseq
             (list #'list-delete-left!))
           fseq))

(defmethod flexible-sequence-delete-left (fseq)
  (funcall (etypecase fseq
             (list #'list-delete-left))
           fseq))

(defmethod flexible-sequence-delete-right! (fseq)
  (funcall (etypecase fseq
             (list #'list-delete-right!))
           fseq))

(defmethod map-equivalence-function (map)
  (funcall (etypecase map
             (<alist-map> #'alist-map-equivalence-function))
           map))

(defmethod map-key-equivalence-function (map)
  (funcall (etypecase map
             (<alist-map> #'alist-map-key-equivalence-function))
           map))

(defmethod map-contains-key? (map key)
  (funcall (etypecase map
             (<alist-map> #'alist-map-contains-key?))
           map
           key))

(defmethod map-keys->list (map)
  (funcall (etypecase map
             (<alist-map> #'alist-map->list))
           map))

(defmethod map-get (map key &optional absence-thunk)
  (apply (etypecase map
           (<alist-map> #'alist-map-get))
         map
         key
         (and absence-thunk (list absence-thunk))))

(defmethod map-put (map key value &optional absence-thunk)
  (apply (etypecase map
           (<alist-map> #'alist-map-put))
         map
         key
         value
         (and absence-thunk (list absence-thunk))))

(defmethod map-put! (map key value &optional absence-thunk)
  (apply (etypecase map
           (<alist-map> #'alist-map-put!))
         map
         key
         value
         (and absence-thunk (list absence-thunk))))

(defmethod map-update (map key func &optional absence-thunk)
  (apply (etypecase map
           (<alist-map> #'alist-map-update))
         map
         key
         func
         (and absence-thunk (list absence-thunk))))

(defmethod map-update! (map key func &optional absence-thunk)
  (apply (etypecase map
           (<alist-map> #'alist-map-update!))
         map
         key
         func
         (and absence-thunk (list absence-thunk))))

(defmethod map-delete (map key)
  (funcall (etypecase map
             (<alist-map> #'alist-map-delete))
           map
           key ))

(defmethod map-delete! (map key)
  (funcall (etypecase map
             (<alist-map> #'alist-map-delete!))
           map
           key ))

(defmethod map-delete-from (map bag)
  (funcall (etypecase map
             (<alist-map> #'alist-map-delete-from))
           map
           bag ))

(defmethod map-delete-from! (map bag)
  (funcall (etypecase map
             (<alist-map> #'alist-map-delete-from!))
           map
           bag ))

(defmethod map-add-from (map source-map)
  (funcall (etypecase map
             (<alist-map> #'alist-map-add-from))
           map
           source-map ))

(defmethod map-add-from! (map source-map)
  (funcall (etypecase map
             (<alist-map> #'alist-map-add-from!))
           map
           source-map ))

(defmethod collection-fold-keys-left (coll fold-function &rest seed)
  (apply (etypecase coll
           (cl:string #'string-fold-keys-left)
           (vector #'vector-fold-keys-left)
           (list #'list-fold-keys-left)
           (<alist-map> #'alist-map-fold-keys-left) )
         coll
         fold-function
         seed ))

(defmethod collection-fold-keys-right (coll fold-function &rest seed)
  (apply (etypecase coll
             (cl:string #'string-fold-keys-right)
             (vector #'vector-fold-keys-right)
             (list #'list-fold-keys-right)
             (<alist-map> #'alist-map-fold-keys-right))
         coll
         fold-function
         seed))

(defmethod collection-fold-left (coll fold-function &rest seed)
  (apply (etypecase coll
             (cl:string #'string-fold-left)
             (vector #'vector-fold-left)
             (list #'list-fold-left)
             (<alist-map> #'alist-map-fold-left))
         coll
         fold-function
         seed))

(defmethod collection-fold-right (coll fold-function &rest seed)
  (apply (etypecase coll
             (cl:string #'string-fold-right)
             (vector #'vector-fold-right)
             (list #'list-fold-right)
             (<alist-map> #'alist-map-fold-right))
         coll
         fold-function
         seed))

(defmethod collection-size (coll)
  (funcall (etypecase coll
             (cl:string #'string-size)
             (vector #'vector-size)
             (list #'list-size)
             (<alist-map> #'alist-map-size))
           coll ))

(defmethod collection-count (coll value)
  (funcall (etypecase coll
             (cl:string #'string-count)
             (vector #'vector-count)
             (list #'list-count)
             (<alist-map> #'alist-map-count))
           coll
           value ))

(defmethod collection-empty? (coll)
  (funcall (etypecase coll
             (cl:string #'string-empty?)
             (vector #'vector-empty?)
             (list #'list-empty?)
             (<alist-map> #'alist-map-empty?))
           coll ))

(defmethod collection-get-any (coll &optional maybe-ft)
  (apply (etypecase coll
           (cl:string #'string-get-any)
           (vector #'vector-get-any)
           (list #'list-get-any)
           (<alist-map> #'alist-map-get-any))
         coll
         (and maybe-ft (list maybe-ft))))

(defmethod collection-copy (coll)
  (funcall (etypecase coll
             (cl:string #'string-copy)
             (vector #'vector-copy)
             (list #'list-copy)
             (<alist-map> #'alist-map-clear))
           coll ))

(defmethod collection-clear (coll)
  (funcall (etypecase coll
             (cl:string #'string-clear)
             (vector #'vector-clear)
             (list #'list-clear)
             (<alist-map> #'alist-map-clear))
           coll ))

(defmethod collection-clear! (coll)
  (funcall (etypecase coll
             (cl:string #'string-clear!)
             (vector #'vector-clear!)
             (list #'list-clear!)
             (<alist-map> #'alist-map-clear!))
           coll ))

(defmethod collection=-2op (pred col1 col2)
  (funcall (etypecase col1
             (cl:string #'string=)
             (vector #'vector=)
             (list #'list=) )
           pred col1 col2))

(defmethod bag-equivalence-function (object)
  (funcall (etypecase object
             (cl:string #'string-equivalence-function)
             (vector #'vector-equivalence-function)
             (list #'list-equivalence-function) )
           object))

(defmethod bag-contains? (bag value)
  (funcall (etypecase bag
             (cl:string #'string-contains?)
             (vector #'vector-contains?)
             (list #'list-contains?) )
           bag
           value))

(defmethod sequence-ref (sequence integer &optional absence-thunk)
  (apply (etypecase sequence
           (cl:string #'string-ref)
           (vector #'vector-ref)
           (list #'list-ref) )
         sequence
         integer
         (and absence-thunk (list absence-thunk)) ))

(defmethod sequence-set (sequence integer value)
  (funcall (etypecase sequence
             (cl:string #'string-set)
             (vector #'vector-set)
             (list #'list-set) )
           sequence
           integer
           value))

(defmethod sequence-set! (sequence integer value)
  (funcall (etypecase sequence
             (cl:string #'string-set!)
             (vector #'vector-set!)
             (list #'list-set!) )
           sequence
           integer
           value))

(defmethod sequence-get-left (sequence &optional absence-thunk)
  (apply (etypecase sequence
           (cl:string #'string-get-left)
           (vector #'vector-get-left)
           (list #'list-get-left) )
         sequence
         (and absence-thunk (list absence-thunk)) ))

(defmethod sequence-get-right (sequence &optional absence-thunk)
  (apply (etypecase sequence
           (cl:string #'string-get-right)
           (vector #'vector-get-right)
           (list #'list-get-right))
         sequence
         (and absence-thunk (list absence-thunk))))

(defmethod sequence-replace-from! ((sequence cl:sequence)
                                  dest-start source-sequence
                                  &optional source-start source-end)
  (apply (etypecase sequence
           (cl:string #'string-replace-from!)
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
           (cl:string #'string-replace-from)
           (vector #'vector-replace-from)
           (list #'list-replace-from))
         sequence
         dest-start
         source-sequence
         `(,@(and source-start (list source-start))
             ,@(and source-end (list source-end) ))))

(defmethod sequence-copy ((sequence cl:sequence) &optional (start 0) end)
  (apply (etypecase sequence
           (cl:string #'string-copy)
           (vector #'vector-copy)
           (list #'list-copy))
         sequence
         start
         (and end (list end))))
