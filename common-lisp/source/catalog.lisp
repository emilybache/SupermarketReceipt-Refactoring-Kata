;;;; catalog.lisp

(in-package :supermarket-receipt)

(defclass supermarket-catalog ()
  ((products :initform nil
             :type list
             :accessor catalog-products)
   (prices :initform nil
           :type list
           :accessor catalog-prices)))

(defmethod add-product-to-catalog ((a-catalog supermarket-catalog) (a-product product) (a-price single-float))
  (error "cannot be called from a unit test - it accesses the database"))

(defmethod unit-price ((a-catalog supermarket-catalog) (a-product product))
  (error "cannot be called from a unit test - it accesses the database"))
