;;;; teller.lisp

(in-package :supermarket-receipt)

(defclass teller ()
        ((catalog :initarg :catalog
                  :type supermarket-catalog
                  :accessor teller-catalog)
         (offers :initform nil
                 :type list
                 :accessor teller-offers)))

(defmethod add-special-offer ((a-teller teller) an-offer-type (a-product product) an-argument)
    (push (cons a-product (make-instance 'offer
                              :type an-offer-type
                              :product a-product
                              :argument an-argument))
          (teller-offers a-teller)))

(defmethod checks-out-articles-from ((a-teller teller) (a-cart shopping-cart))
    (let ((a-receipt (make-instance 'receipt))
          (product-quantities (shopping-cart-items a-cart))
          (a-catalog (teller-catalog a-teller)))
        (mapcar (lambda (a-product-quantity)
                    (let* ((a-product (the-quantities-product a-product-quantity))
                           (a-quantity (quantity a-product-quantity))
                           (a-unit-price (unit-price a-catalog a-product)))
                        (add-product-to-receipt a-receipt
                                                a-product
                                                a-quantity
                                                a-unit-price
                                                (* a-quantity a-unit-price))
                        (format t "~& ~S: ~S" (product-name a-product) (* a-quantity a-unit-price))))
                product-quantities)
        (handle-offers a-cart a-receipt (teller-offers a-teller) a-catalog)
        a-receipt))

(defmethod product-with-name ((a-teller teller) (a-name string))
    (let ((name-with-product (assoc a-name (catalog-products (teller-catalog a-teller)))))
        (when name-with-product
              (cdr name-with-product))))
