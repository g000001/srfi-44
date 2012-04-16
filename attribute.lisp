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

(defgeneric collection-get-any (coll &optional maybe-ft))

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
