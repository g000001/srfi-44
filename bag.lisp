(cl:in-package :srfi-44.internal)

(defclass <bag> (<collection>)
  ((equivalence-function :accessor bag-equivalence-function
               :initarg :equivalence-function
               :initform #'eql)))

(defvar <bag>   (find-class '<bag>))

;;; - Bags -

(defun bag? (obj)
  (typep obj <bag>))

(define-supertype-handled bag?
  ((bag-size bag)
   collection-size)
  ((bag-count bag value)
   collection-count)
  ((bag-get-any bag . maybe-fk)
   collection-get-any)
  ((bag-empty? bag)
   collection-empty?)
  ((bag->list bag)
   collection->list)
  ((bag-clear bag)
   collection-clear)
  ((bag-clear! bag)
   collection-clear!)
  ((bag-copy bag)
   collection-copy))

(defgeneric bag-contains? (bag value))
(defgeneric bag-add (bag value))
(defgeneric bag-add! (bag value))
(defgeneric bag-delete (bag value))
(defgeneric bag-delete! (bag value))
(defgeneric bag-delete-all (bag value))
(defgeneric bag-delete-all! (bag value))
(defgeneric bag-add-from (bag source-bag))
(defgeneric bag-add-from! (bag source-bag))
(defgeneric bag-delete-from (bag source-bag))
(defgeneric bag-delete-from! (bag source-bag))
(defgeneric bag-delete-all-from (bag source-bag))
(defgeneric bag-delete-all-from! (bag source-bag))

(defmethod bag-add! ((bag <bag>) value)
  (push value (contents bag))
  bag)

(defmethod bag-add ((bag <bag>) value)
  (bag-add! (bag-copy bag) value))


(defmethod bag-add-from! ((bag <bag>) (source-bag <bag>))
  (setf (contents bag)
        (nconc (contents bag) (contents source-bag)))
  bag)

(defmethod bag-add-from ((bag <bag>) (source-bag <bag>))
  (bag-add-from (bag-copy bag) source-bag))

(defmethod bag-delete! ((bag <bag>) value)
  (setf (contents bag)
        (delete value (contents bag) :count 1))
  bag)

(defmethod bag-delete ((bag <bag>) value)
  (bag-delete! (bag-copy bag) value))

(defmethod bag-delete-all! ((bag <bag>) value)
  (setf (contents bag)
        (delete value (contents bag)))
  bag)

(defmethod bag-delete-all ((bag <bag>) value)
  (bag-delete-all! (bag-copy bag) value))

(defmethod bag-delete-from! ((bag <bag>) (source-bag <bag>))
  (dolist (e (contents source-bag))
    (bag-delete! bag e))
  bag)

(defmethod bag-delete-from ((bag <bag>) (source-bag <bag>))
  (bag-delete-from! (bag-copy bag) source-bag))

(defmethod bag-delete-all-from! ((bag <bag>) (source-bag <bag>))
  (dolist (e (contents source-bag))
    (bag-delete-all! bag e))
  bag)

(defmethod bag-delete-all-from ((bag <bag>) (source-bag <bag>))
  (bag-delete-all-from! (bag-copy bag) source-bag))

(defmethod bag-contains? ((bag <bag>) value)
  (let ((elt=? (bag-equivalence-function bag)))
    (collection-fold-left bag
                          (lambda (elt _)
                            (declare (ignore _))
                            (if (funcall elt=? elt value)
                                (values 'NIL 'T)
              (values 'T 'NIL)))
                          'NIL)))

(define-function (bag= elt=? . bags)
  (for-each (lambda (bag)
              (check-arg #'bag? bag 'bag=)) bags)
  (apply #'collection= elt=? bags))

;;; eof
