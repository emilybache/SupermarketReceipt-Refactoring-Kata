;;;; shopping-cart.lisp

(in-package :supermarket-receipt)

(defclass shopping-cart ()
        ((items :initform nil
                :type list
                :accessor shopping-cart-items)
         (product-quantities :initform nil
                    :type list
                    :accessor shopping-cart-product-quantities)))

(defmethod add-item ((a-cart shopping-cart) (an-item product))
    (add-item-quantity a-cart an-item 1.0))

(defmethod add-item-quantity ((a-cart shopping-cart) (an-item product) (a-quantity single-float))
    (push (make-instance 'product-quantity
              :product an-item
              :quantity a-quantity)
          (shopping-cart-items a-cart))
    (if (assoc an-item (shopping-cart-product-quantities a-cart))
        (incf (cdr (assoc an-item (shopping-cart-product-quantities a-cart))) a-quantity)
        (push (cons an-item a-quantity) (shopping-cart-product-quantities a-cart))))

(defmethod handle-offers ((a-cart shopping-cart) (a-receipt receipt) (offers list) (a-catalog supermarket-catalog))
  (let* ((product-quantities (shopping-cart-product-quantities a-cart))
         (products (mapcar #'car product-quantities)))
    (mapcar (lambda (a-product)
              (let ((a-quantity (cdr (assoc a-product product-quantities)))
                    (offer-for-product (cdr (assoc a-product offers))))
                (when offer-for-product
                  (let ((a-unit-price (unit-price a-catalog a-product))
                        (floored-quantity (floor a-quantity))
                        (a-discount nil)
                        (x 1)
                        (the-offer-type (offer-type offer-for-product)))
                    (if (eq the-offer-type 'three-for-two)
                        (setf x 3)
                        (when (eq the-offer-type 'two-for-amount)
                          (setf x 2)
                          (when (>= floored-quantity 2)
                            (let* ((total (+ (* (offer-argument offer-for-product) (floor (/ floored-quantity x)))
                                             (* (mod floored-quantity 2) a-unit-price)))
                                   (discount-n (- (* a-unit-price a-quantity) total)))
                              (setf a-discount (make-instance 'discount 
                                                              :product a-product
                                                              :description (format nil "2 for ~S" (offer-argument offer-for-product))
                                                              :amount (- discount-n)))))))
                    (when (eq the-offer-type 'five-for-amount)
                      (setf x 5))
                    (let ((number-of-x (floor (/ floored-quantity x))))
                      (when (and (eq the-offer-type 'three-for-two)
                                 (> floored-quantity 2))
                        (let ((discount-amount (- (* a-quantity a-unit-price)
                                                  (+ (* number-of-x 2 a-unit-price)
                                                     (* (mod floored-quantity 3) a-unit-price)))))
                          (setf a-discount (make-instance 'discount
                                                          :product a-product
                                                          :description "3 for 2"
                                                          :amount (- discount-amount)))))
                      (when (eq the-offer-type 'ten-percent-discount)
                        (setf a-discount (make-instance 'discount
                                                        :product a-product
                                                        :description (format nil "~S % off" (offer-argument offer-for-product))
                                                        :amount (/ (* (- a-quantity) a-unit-price (offer-argument offer-for-product)) 100.0))))
                      (when (and (eq the-offer-type 'five-for-amount)
                                 (>= floored-quantity 5))
                        (let ((discount-total (- (* a-quantity a-unit-price)
                                                 (+ (* (offer-argument offer-for-product) number-of-x)
                                                    (* (mod floored-quantity 5) a-unit-price)))))
                          (setf a-discount (make-instance 'discount
                                                          :product a-product
                                                          :description (format nil "~S for ~S" x (offer-argument offer-for-product))
                                                          :amount (- discount-total)))))
                      (when a-discount
                        (add-discount a-receipt a-discount)))))))
            products)))
