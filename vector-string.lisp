(cl:in-package :srfi-44.internal)

;;; - Vectors and Strings -

;; vector -> cl:vector
;;
(defun string (&rest args)
  (cl:coerce args 'cl:string))

(defun make-string (size &optional (init #\Nul))
  (cl:make-string size :initial-element init))


(defmethod collection-name ((v cl:vector))
  'cl:vector)

(defmethod collection-name ((s cl:string))
  'cl:string)

(define-function vector-size #'vector-length)
(define-function string-size #'string-length)

(define-function (vector-folder vector-ref select-start select-end select-next)
  (letrec
      ((fold
        (lambda (seq kons . knils)
          (let ((knil-count (list-size knils))
                (start (funcall select-start seq))
                (end (funcall select-end seq)))
            (iterate loop ((knils knils) (i start))
              (if (= i end)
                  (apply #'values knils)
                  (receive (proceed? . new-knils)
                      (apply kons i (funcall vector-ref seq i) knils)
                    (cond ((not (= (list-size new-knils) knil-count))
                           (error "Too many knils"
                                  fold
                                  `(expected ,knil-count)
                                  `(got ,new-knils)))
                          (proceed?
                           (loop new-knils (funcall select-next i)))
                          (:else
                           (apply #'values knils))))))))))
    fold))

(define-function vector-fold-keys-left
  (vector-folder #'cl:aref
    (always 0)             ;select-start
    #'vector-size            ;select-end
    (lambda (i) (+ i 1)))) ;select-next
(define-function vector-fold-keys-right
  (vector-folder #'cl:aref
    (lambda (vec)          ;select-start
      (- (vector-size vec) 1))
    (always -1)            ;select-end
    (lambda (i) (- i 1)))) ;select-next
(define-function (vector-fold-left vec kons . knils)
  (apply #'vector-fold-keys-left vec
         (lambda (index elt . knils)
           (declare (ignore index))
           (apply kons elt knils))
         knils))
(define-function (vector-fold-right vec kons . knils)
  (apply #'vector-fold-keys-right vec
         (lambda (index elt . knils)
           (declare (ignore index))
           (apply kons elt knils))
         knils))

(define-function string-fold-keys-left
  (vector-folder #'cl:char
    (always 0)               ;select-start
    #'string-size              ;select-end
    (lambda (i) (+ i 1)))) ;select-next
(define-function string-fold-keys-right
  (vector-folder #'cl:char
    (lambda (s)            ;select-start
      (- (string-size s) 1))
    (always -1)            ;select-end
    (lambda (i) (- i 1)))) ;select-next
(define-function (string-fold-left string kons . knils)
  (apply #'string-fold-keys-left string
         (lambda (index elt . knils)
           (declare (ignore index))
           (apply kons elt knils))
         knils))
(define-function (string-fold-right string kons . knils)
  (apply #'string-fold-keys-right string
         (lambda (index elt . knils)
           (declare (ignore index))
           (apply kons elt knils))
         knils))

(define-function (vector-equivalence-function vec)
  (declare (ignore vec))
  #'eqv?)
(define-function (string-equivalence-function str)
  (declare (ignore str))
  #'char=?)

(define-function (vector-empty? vector)
  (zero? (vector-size vector)))
(define-function (string-empty? string)
  (zero? (string-size string)))

(define-function (vlike-copier make-x x-size x-ref x-set!)
  (rec (copy vec . maybe-start+end)
    (let*-optionals #'copy maybe-start+end
        ((start 0) (end (funcall x-size vec)))
      (do ((new (funcall make-x (- end start)))
           (i start (+ i 1))
           (j 0 (+ j 1)))
          ((= i end) new)
        (funcall x-set! new j (funcall x-ref vec i))))))
(define-function vector-copy (vlike-copier #'make-vector #'vector-size
                                  #'cl:aref #'vector-set!))
(define-function string-copy (vlike-copier #'make-string #'string-size
                                  #'cl:char #'string-set!))

;; R5RS defines both VECTOR->LIST and STRING->LIST.

(define-function (vlike-contains? x-size x-ref elt=?)
  (lambda (vec value)
    (let ((size (funcall x-size vec)))
      (iterate loop ((i 0))
        (cond ((= i size)
               'NIL)
              ((funcall elt=? (funcall x-ref vec i) value)
               'T)
              (:else
               (loop (+ i 1))))))))
(define-function vector-contains?
  (vlike-contains? #'vector-size #'cl:aref #'eqv?))
(define-function string-contains?
  (vlike-contains? #'string-size #'cl:char #'char=?))

(define-function (vlike-count x-size x-ref elt=?)
  (lambda (vec value)
    (do ((size (funcall x-size vec))
         (i 0 (+ i 1))
         (c 0 (if (funcall elt=? (funcall x-ref vec i) value)
                  (+ c 1)
                  c)))
        ((= i size) c))))
(define-function vector-count (vlike-count #'vector-size #'cl:aref #'eqv?))
(define-function string-count (vlike-count #'string-size #'cl:char #'char=?))

(define-function (vlike-ref x-size x-ref)
  (rec (x-ref* vec k . maybe-ft)
    (cond ((null? maybe-ft)
           (funcall x-ref vec k))
          ((null? (cdr maybe-ft))
           (if (or (< k 0) (>= k (funcall x-size vec)))
               (funcall (car maybe-ft))
               (funcall x-ref vec k)))
          (:else
           (apply #'error "Too many arguments" x-ref*
                  vec k
                  maybe-ft)))))
(define-function vector-ref (vlike-ref #'vector-size #'cl:aref))
(define-function string-ref (vlike-ref #'string-size #'cl:char))

(define-function (vlike-get-left x-size x-ref)
  (rec (x-get-right vec . maybe-ft)
    (cond ((null? maybe-ft)
           (funcall x-ref vec 0))
          ((null? (cdr maybe-ft))
           (if (zero? (funcall x-size vec))
               (funcall (car maybe-ft))
               (funcall x-ref vec 0)))
          (:else
           (error "Too many arguments" x-get-right
                  vec
                  maybe-ft)))))
(define-function vector-get-left (vlike-get-left #'vector-size #'cl:aref))
(define-function string-get-left (vlike-get-left #'string-size #'cl:char))

(define-function vector-get-any #'vector-get-left)
(define-function string-get-any #'string-get-left)

(define-function (vlike-get-right x-size x-ref)
  (rec (x-get-right vec . maybe-ft)
    (cond ((null? maybe-ft)
           (funcall x-ref vec (- (funcall x-size vec) 1)))
          ((null? (cdr maybe-ft))
           (if (zero? (funcall x-size vec))
               (funcall (car maybe-ft))
               (funcall x-ref vec (- (funcall x-size vec) 1))))
          (:else
           (error "Too many arguments" x-get-right
                  vec
                  maybe-ft)))))
(define-function vector-get-right (vlike-get-right #'vector-size #'cl:aref))
(define-function string-get-right (vlike-get-right #'string-size #'cl:char))

(define-function (vlike-set x-copy x-set!)
  (lambda (vec k value)
    (let ((copy (funcall x-copy vec)))
      (funcall x-set! vec k value)
      copy)))
(define-function vector-set (vlike-set #'vector-copy #'vector-set!))
(define-function string-set (vlike-set #'string-copy #'string-set!))

(define-function (vector-clear  vec)
  (declare (ignore vec))
  (make-vector 0))
(define-function (vector-clear! vec)
  (vector-fill! vec 'NIL) (make-vector 0))

(define-function (string-clear s)
  (declare (ignore s))
  (make-string 0))
(define-function (string-clear! s)
  (declare (ignore s))
  (make-string 0))

(define-function (vlike-replace-from! x-size x-ref x-set!)
  (rec (x-replace-from! target tstart source . maybe-sstart+send)
    (let*-optionals #'x-replace-from! maybe-sstart+send
        ((sstart 0) (send (funcall x-size source)))
      ;; To hell with bounds checking.
      (do ((i tstart (+ i 1))
           (j sstart (+ j 1)))
          ((= j send))
        (funcall x-set! target i (funcall x-ref source j))))))
(define-function vector-replace-from!
  (vlike-replace-from! #'vector-size #'cl:aref #'vector-set!))
(define-function string-replace-from!
  (vlike-replace-from! #'string-size #'cl:char #'string-set!))

(define-function (vlike-replace-from x-copy x-replace-from!)
  (lambda (target tstart source . maybe-sstart+send)
    (let ((copy (funcall x-copy target)))
      (apply x-replace-from! copy tstart source maybe-sstart+send)
      copy)))
(define-function vector-replace-from
  (vlike-replace-from #'vector-copy #'vector-replace-from!))
(define-function string-replace-from
  (vlike-replace-from #'string-copy #'string-replace-from!))

(define-function (vlike= x-size x-ref)
  (flet ((binary= ( elt=? v1 v2)
           (let ((s1 (funcall x-size v1)))
             (and (= s1 (funcall x-size v2))
                  (iterate loop ((i 0))
                           (or (= i s1)
                               (and (funcall elt=?
                                             (funcall x-ref v1 i)
                                             (funcall x-ref v2 i))
                                    (loop (+ i 1)))))))))
    (rec (x= elt=? . vecs)
         (or (null? vecs) (null? (cdr vecs))
             (and (binary= elt=? (car vecs) (cadr vecs))
                  (apply x= elt=? (cdr vecs)))))))

(define-function vector= (vlike= #'vector-size #'cl:aref))
(define-function string= (vlike= #'string-size #'cl:char))
