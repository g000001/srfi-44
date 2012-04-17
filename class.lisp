(cl:in-package :srfi-44.internal)

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
