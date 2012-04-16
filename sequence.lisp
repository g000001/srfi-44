(cl:in-package :srfi-44.internal)

(defclass <sequence> (<bag>) ())
(defvar <sequence> (find-class '<bag>))

;;; - Sequences -

(defun sequence? (obj)
  (typep obj <sequence>))

(defgeneric sequence-ref (sequence integer &optional absence-thunk))
(defgeneric sequence-get-left (sequence &optional absence-thunk))
(defgeneric sequence-get-right (sequence &optional absence-thunk))
(defgeneric sequence-insert-right (sequence value))
(defgeneric sequence-insert-right! (sequence value))
(defgeneric sequence-set (sequence integer value))
(defgeneric sequence-set! (sequence integer value))
;; (defgeneric sequence-add (sequence value))
;; (defgeneric sequence-add! (sequence value))
(defgeneric sequence-replace-from (sequence dest-start source-sequence
                                     &optional source-start source-end))
(defgeneric sequence-replace-from! (sequence dest-start source-sequence
                                     &optional source-start source-end))
(defgeneric sequence-copy (sequence &optional start end))

(define-supertype-handled sequence?
  ((sequence-size seq)
   collection-size)
  ((sequence-count seq value)
   collection-count)
  ((sequence-get-any seq . maybe-fk)
   collection-get-any)
  ((sequence-empty? seq)
   collection-empty?)
  ((sequence->list seq)
   collection->list)
  ((sequence-clear seq)
   collection-clear)
  ((sequence-clear! seq)
   collection-clear!)

  ((sequence-equivalence-function seq)
   bag-equivalence-function)
  ((sequence-contains? seq value)
   bag-contains?)
  ((sequence-add seq value)
   bag-add)
  ((sequence-add! seq value)
   bag-add!)
  ((sequence-delete seq value)
   bag-delete)
  ((sequence-delete! seq value)
   bag-delete!)
  ((sequence-delete-all seq value)
   bag-delete-all)
  ((sequence-delete-all! seq value)
   bag-delete-all!)
  ((sequence-delete-all-from seq bag)
   bag-delete-all-from)
  ((sequence-delete-all-from! seq bag)
   bag-delete-all-from!)
  ((sequence-add-from seq value)
   bag-add-from)
  ((sequence-add-from! seq value)
   bag-add-from!)
  ((sequence-delete-from seq value)
   bag-delete-from)
  ((sequence-delete-from! seq value)
   bag-delete-from!))

(defmethod collection-fold-right ((seq <sequence>) f &rest seeds)
  (let ((size (sequence-size seq))
        (seed-count (list-size seeds)) )
    (srfi-5:let loop ((seeds seeds) (i (- size 1)))
                (if (negative? i)
                    (apply #'values seeds)
                    (receive (proceed? . new-seeds)
                             (apply f seeds)
                      (if (= (list-size new-seeds) seed-count)
                          (if proceed?
                              (loop new-seeds (- i 1))
                              (apply #'values new-seeds) )
                          (error "(sequence)-fold-right: Wrong seed count"
                                 `(expected ,seed-count)
                                 `(got ,(list-size new-seeds)) )))))))

(defmethod sequence-ref ((seq <sequence>) (index integer)
                         &optional (absence-thunk (constantly 'NIL)))
  (if (null (contents seq))
      (funcall absence-thunk)
      (elt (contents seq) index)))

(defmethod collection-get-any ((seq <sequence>) &optional maybe-ft)
  (if (zero? (sequence-size seq))
      (if (pair? maybe-ft) (funcall (car maybe-ft)) 'NIL)
      (sequence-ref seq 0)))

(defmethod sequence-copy ((seq <sequence>) &optional (start 0) end)
  (cl:subseq (bag-copy seq) start end))

(defmethod sequence-insert-right! ((seq <sequence>) value)
  (setf (contents seq)
        (rplacd (last (contents seq))
                (list value)))
  seq)

(defmethod sequence-insert-right ((seq <sequence>) value)
  (sequence-insert-right! (sequence-copy seq) value))

(defmethod sequence-insert-left! ((seq <sequence>) value)
  (push value (contents seq))
  seq)

(defmethod sequence-insert-left ((seq <sequence>) value)
  (sequence-insert-left! (sequence-copy seq) value))

(defmethod sequence-get-left ((seq <sequence>) &optional maybe-fk)
  (if (collection-empty? seq)
      (and maybe-fk (funcall (car maybe-fk)))
      (sequence-ref seq 0)))

(defmethod sequence-get-right ((seq <sequence>) &optional maybe-fk)
  (if (collection-empty? seq)
      (and maybe-fk (funcall (car maybe-fk)))
      (sequence-ref seq (- (collection-size seq) 1))))

(defmethod sequence-set! ((seq <sequence>) k value)
  (setf (elt (contents seq) k)
        value)
  seq)

(defmethod sequence-set ((seq <sequence>) k value)
  (sequence-set! (sequence-copy seq) k value))

(defmethod sequence-replace-from! ((seq <sequence>)
                                   dstart
                                   (source <sequence>)
                                   &optional (sstart 0) send)
  (replace (contents seq)
           (contents source)
           :start1 dstart
           :start2 sstart
           :end1 send))

(defmethod sequence-replace-from  ((seq <sequence>)
                                   dstart
                                   (source <sequence>)
                                   &optional (sstart 0) send)
  (sequence-replace-from! (sequence-copy seq)
                          dstart
                          source
                          sstart
                          send))

(define-function (sequence= elt=? . sequences)
  (for-each (lambda (sequence)
              (check-arg #'sequence? sequence 'sequence=)) sequences)
  (apply #'collection= elt=? sequences))
