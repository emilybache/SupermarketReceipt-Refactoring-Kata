;;;; fake-catalog.lisp

(in-package :common-lisp-user)

(defpackage :supermarket-receipt/fake-catalog
  (:use :common-lisp
        :supermarket-receipt)
  (:export :fake-catalog
           :add-product-to-catalog
           :unit-price))

(in-package :supermarket-receipt/fake-catalog)

(defclass fake-catalog (supermarket-catalog)
  ())

(defmethod add-product-to-catalog ((a-catalog fake-catalog) (a-product product) (a-price single-float))
  (push (cons (product-name a-product) a-product) (catalog-products a-catalog))
  (push (cons (product-name a-product) a-price) (catalog-prices a-catalog)))

(defmethod unit-price ((a-catalog fake-catalog) (a-product product))
  (cdr (assoc (product-name a-product) (catalog-prices a-catalog))))
