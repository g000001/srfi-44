;;;; srfi-44.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)

(defsystem :srfi-44
  :serial t
  :depends-on (:fiveam
               :srfi-2
               :srfi-23
               :mbe
               ;; :tiny-clos
               )
  :components ((:file "package")
               (:file "util")
               ;; (:file "utilities")
               ;; (:file "support")
               ;; (:file "tiny-clos")
               ;; (:file "utilities_tiny-clos")
               ;; (:file "srfi-44_tiny-clos")
               (:file "class")
               (:file "list")
               (:file "vector-string")
               (:file "alist")
               (:file "collection")
               (:file "bag")
               (:file "set")
               (:file "map")
               (:file "sequence")
               (:file "flexible-sequence")
               (:file "attribute")
               (:file "srfi-44")))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-44))))
  (load-system :srfi-44)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :srfi-44.internal :srfi-44))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))
