;;;; model-objects.lisp

(in-package :supermarket-receipt)

(deftype product-unit () '(member each kilo))

(defclass product ()
        ((name :initarg :name
               :type string
               :accessor product-name)
         (unit :initarg :unit
               :type product-unit
               :accessor the-product-unit)))

(defclass product-quantity ()
        ((product :initarg :product
                  :type product
                  :accessor the-quantities-product)
         (quantity :initarg :quantity
                   :type single-float
                   :accessor quantity)))

(deftype special-offer-type () '(member three-for-two ten-percent-discount two-for-amount five-for-amount))

(defclass offer ()
        ((offer-type :initarg :type
                     :type special-offer-type
                     :accessor offer-type)
         (product :initarg :product
                  :type product
                  :accessor offered-product)
         (argument :initarg :argument
                   :accessor offer-argument)))

(defclass discount ()
        ((product :initarg :product
                  :type product
                  :accessor discounted-product)
         (description :initarg :description
                      :type string
                      :accessor discount-description)
         (amount :initarg :amount
                 :type single-float
                 :accessor discount-amount)))
