;; Copyright (C) 2003 Taylor Campbell and Scott G. Miller.  See the
;; LICENCE file for details.

;; This reference implementation uses Gregor Kiczales' Tiny-CLOS.

;; This file requires utilities.scm, utilities_tiny-clos.scm, SRFI 2
;; (and-let*), SRFI 23 (error), and Tiny-CLOS.

(cl:in-package :srfi-44.internal)


;;; - Attributes -

(defgeneric collection-get-any (coll &optional maybe-ft))

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
