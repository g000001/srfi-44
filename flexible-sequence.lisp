(cl:in-package :srfi-44.internal)

(defclass <flexible-sequence> (<sequence>) ())

(defvar <flexible-sequence> (find-class '<sequence>))

;;; - Flexible Sequences -

(defun flexible-sequence? (obj)
  (typep obj <flexible-sequence>))

(define-supertype-handled flexible-sequence?
  ((flexible-sequence-size fseq)
   collection-size)
  ((flexible-sequence-count fseq value)
   collection-count)
  ((flexible-sequence-get-any fseq . maybe-fk)
   collection-get-any)
  ((flexible-sequence-empty? fseq)
   collection-empty?)
  ((flexible-sequence->list fseq)
   collection->list)
  ((flexible-sequence-clear fseq)
   collection-clear)
  ((flexible-sequence-clear! fseq)
   collection-clear!)
  #|((flexible-sequence-copy fseq) ???
   collection-copy)|#

  ((flexible-sequence-equivalence-function fseq)
   bag-equivalence-function)
  ((flexible-sequence-contains? fseq value)
   bag-contains?)
  ((flexible-sequence-delete fseq value)
   bag-delete)
  ((flexible-sequence-delete! fseq value)
   bag-delete!)
  ((flexible-sequence-delete-from fseq value)
   bag-delete-from)
  ((flexible-sequence-delete-from! fseq value)
   bag-delete-from!)
  ((flexible-sequence-delete-all fseq value)
   bag-delete-all)
  ((flexible-sequence-delete-all! fseq value)
   bag-delete-all!)
  ((flexible-sequence-delete-all-from fseq bag)
   sequence-delete-all-from)
  ((flexible-sequence-delete-all-from! fseq bag)
   sequence-delete-all-from!)
  ((flexible-sequence-ref fseq k)
   sequence-ref)
  ((flexible-sequence-get-left fseq)
   sequence-get-left)
  ((flexible-sequence-get-right fseq)
   sequence-get-right)
  ((flexible-sequence-set fseq k value)
   sequence-set)
  ((flexible-sequence-set! fseq k value)
   sequence-set!)
  ((flexible-sequence-insert-right fseq value)
   sequence-insert-right)
  ((flexible-sequence-insert-right! fseq value)
   sequence-insert-right!)
  ((flexible-sequence-add fseq value)
   sequence-add)
  ((flexible-sequence-add! fseq value)
   sequence-add!)
  ((flexible-sequence-add-from fseq bag)
   sequence-add-from)
  ((flexible-sequence-add-from! fseq bag)
   sequence-add-from!)
  ((flexible-sequence-replace-from fseq dstart source . sstart+send)
   sequence-replace-from)
  ((flexible-sequence-replace-from! fseq dstart source . sstart+send)
   sequence-replace-from!)
  ((flexible-sequence-copy fseq . start+end)
   sequence-copy))

(define-function (flexible-sequence= elt=? . flexible-sequences)
  (for-each (lambda (flexible-sequence) (check-arg #'flexible-sequence?
                                                   flexible-sequence
                                                   'flexible-sequence=))
            flexible-sequences)
  (apply #'sequence= elt=? flexible-sequences))

(defgeneric flexible-sequence-insert (fseq index value))
(defgeneric flexible-sequence-insert! (fseq index value))
(defgeneric flexible-sequence-delete-at (fseq index))
(defgeneric flexible-sequence-delete-at! (fseq index))
(defgeneric flexible-sequence-insert-left (fseq value))
(defgeneric flexible-sequence-insert-left! (fseq value))
(defgeneric flexible-sequence-delete-left (fseq value))
(defgeneric flexible-sequence-delete-left! (fseq value))
(defgeneric flexible-sequence-delete-right (fseq value))
(defgeneric flexible-sequence-delete-right! (fseq value))
