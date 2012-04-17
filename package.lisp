;;;; package.lisp

(cl:in-package :cl-user)

;; (g1::delete-package*  :srfi-44)

(defpackage :srfi-44
  (:use)
  (:import-from :cl :list :vector)
  (:export
   :collection-name
   :make-collection :make-bag :make-set :make-map :make-sequence
   :make-flexible-sequence
   :collection :bag :set :map :sequence :flexible-sequence
   :collection-fold-left :collection-fold-right :collection-fold-keys-left
   :collection-fold-keys-right :collection? :collection-size :collection-count
   :collection-get-any :collection-empty? :collection->list :collection-clear
   :collection-clear! :collection= :collection-copy :bag-fold-left
   :bag-fold-right :bag-fold-keys-left :bag-fold-keys-right :bag? :bag-size
   :bag-count :bag-get-any :bag-empty? :bag->list :bag-clear :bag-clear! :bag=
   :bag-copy :set-fold-left :set-fold-right :set-fold-keys-left
   :set-fold-keys-right :set? :set-size :set-count :set-get-any :set-empty?
   :set->list :set-clear :set-clear! :set= :set-copy :map-fold-left
   :map-fold-right :map-fold-keys-left :map-fold-keys-right :map? :map-size
   :map-count :map-get-any :map-empty? :map->list :map-clear :map-clear! :map=
   :map-copy :sequence-fold-left :sequence-fold-right :sequence-fold-keys-left
   :sequence-fold-keys-right :sequence? :sequence-size :sequence-count
   :sequence-get-any :sequence-empty? :sequence->list :sequence-clear
   :sequence-clear! :sequence= :sequence-copy :flexible-sequence-fold-left
   :flexible-sequence-fold-right :flexible-sequence-fold-keys-left
   :flexible-sequence-fold-keys-right :flexible-sequence? :flexible-sequence-size
   :flexible-sequence-count :flexible-sequence-get-any :flexible-sequence-empty?
   :flexible-sequence->list :flexible-sequence-clear :flexible-sequence-clear!
   :flexible-sequence= :flexible-sequence-copy)
  (:export
   :ordered-collection?
   :collection-ordering-function :collection-get-left :collection-get-right
   :collection-delete-left :collection-delete-left! :collection-delete-right
   :collection-delete-right! )
  (:export
   :limited-collection? )
  (:export
   :purely-mutable-collection? )
  (:export
   :directional-collection?
   :collection-get-left :collection-get-right :collection-insert-left
   :collection-insert-left! )
  (:export
   :bag?
   :bag-equivalence-function :bag-contains? :bag-add :bag-add! :bag-delete
   :bag-delete! :bag-delete-all :bag-delete-all! :bag-add-from :bag-add-from!
   :bag-delete-from :bag-delete-from! :bag-delete-all-from :bag-delete-all-from!)
  (:export
   :set?
   :set-equivalence-function :set-contains? :set-subset? :set-add :set-add!
   :set-delete :set-delete! :set-union :set-union! :set-intersection
   :set-intersection! :set-difference :set-difference! :set-symmetric-difference
   :set-symmetric-difference! :set-add-from :set-add-from! :set-delete-from
   :set-delete-from! )
  (:export
   :sequence?
   :sequence-ref :sequence-get-left :sequence-get-right :sequence-insert-right
   :sequence-insert-right! :sequence-set :sequence-set! :sequence-add
   :sequence-add! :sequence-replace-from :sequence-replace-from! :sequence-copy)
  (:export
   :flexible-sequence?
   :flexible-sequence-insert :flexible-sequence-insert!
   :flexible-sequence-delete-at :flexible-sequence-delete-at!
   :flexible-sequence-insert-left :flexible-sequence-insert-left!
   :flexible-sequence-insert-right :flexible-sequence-insert-right!
   :flexible-sequence-delete-left :flexible-sequence-delete-left!
   :flexible-sequence-delete-right :flexible-sequence-delete-right!)
  (:export
   :map?
   :map-equivalence-function :map-key-equivalence-function :map-contains-key?
   :map-keys->list :map-get :map-put :map-put! :map-update :map-update!
   :map-delete :map-delete! :map-delete-from :map-delete-from! :map-add-from
   :map-add-from!)
  (:export
   :make-list :list :list-fold-left :list-fold-right
   :list-equivalence-function :list-copy :list->list :list? :list-size
   :list-empty? :list-contains? :list-ref :list-get-any :list-get-left
   :list-get-right :list-count :list= :list-add :list-add! :list-set :list-set!
   :list-insert-left :list-insert-left! :list-insert-right :list-insert-right!
   :list-delete :list-delete! :list-delete-left :list-delete-left!
   :list-delete-right :list-delete-right! :list-delete-all :list-delete-all!
   :list-add-from :list-add-from! :list-delete-from :list-delete-from!
   :list-delete-all-from :list-delete-all-from! :list-replace-from
   :list-replace-from! :list-clear :list-clear! :list-insert :list-insert!
   :list-delete-at :list-delete-at!)
  (:export
   :make-alist-map :alist-map :alist-map-fold-left :alist-map-fold-right
   :alist-map-fold-keys-left :alist-map-fold-keys-right
   :alist-map-equivalence-function :alist-map-key-equivalence-function
   :alist-map-count :alist-map-key-count :alist-map-contains-key? :alist-map-size
   :alist-map-copy :alist-map->list :alist-map-keys->list :alist-map? :alist-map=
   :alist-map-get :alist-map-get-all :alist-map-put :alist-map-put!
   :alist-map-replace-all :alist-map-replace-all! :alist-map-update
   :alist-map-update! :alist-map-update-all :alist-map-update-all!
   :alist-map-delete :alist-map-delete! :alist-map-delete-all
   :alist-map-delete-all! :alist-map-delete-from :alist-map-delete-from!
   :alist-map-delete-all-from :alist-map-delete-all-from! :alist-map-clear
   :alist-map-clear! :alist-map-add-from :alist-map-add-from!)
  (:export
   :make-vector :vector :vector-fold-left :vector-fold-right
   :vector-equivalence-function :vector-copy :vector->list :vector? :vector-size
   :vector-empty? :vector-contains? :vector-count :vector-ref :vector-get-any
   :vector-get-left :vector-get-right :vector-set :vector-set!
   :vector-replace-from :vector-replace-from! :vector=)
  (:export
   :string
   :make-string :string :string-fold-left :string-fold-right
   :string-equivalence-function :string-copy :string->list :string-size
   :string-empty? :string? :string-contains? :string-count :string-ref
   :string-get-any :string-get-left :string-get-right :string-set :string-set!
   :string-replace-from :string-replace-from! :string=))

(defpackage :srfi-44.internal
  (:use :srfi-44 :cl :fiveam :mbe
        :srfi-2)
  (:shadowing-import-from :srfi-23 :error)
  (:shadowing-import-from :srfi-5 :let)
  #|(:shadowing-import-from :tiny-clos
                          :class-of
                          :make-method
                          :allocate-instance
                          :add-method)|#
  (:shadow :lambda :member :assoc :map :loop :method ;; :class-of
           :list*
           :EVERY
           :REMOVE
           :UNION
           :REMOVE-DUPLICATES
           ;;
           ;:class-of
           ;:make-method
           ;:allocate-instance
           ;:add-method
           )
  (:shadowing-import-from :srfi-44
                          :make-list :set :make-sequence :make-string
                          :string= :vector :string :set-difference
                          :list :sequence
                          ;; :map
                          ))
