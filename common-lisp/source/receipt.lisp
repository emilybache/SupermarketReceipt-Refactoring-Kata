;;;; receipt.lisp

(in-package :supermarket-receipt)

(defclass receipt-item ()
  ((product :initarg :product
            :type product
            :accessor item-product)
   (quantity :initarg :quantity
             :type product-quantity
             :accessor item-quantity)
   (price :initarg :price
          :type single-float
          :accessor item-price)
   (total-price :initarg :total-price
                :type single-float
                :accessor item-total-price)))

(defclass receipt ()
  ((items :initform nil
          :type list
          :accessor receipt-items)
   (discounts :initform nil
              :type list
              :accessor receipt-discounts)))

(defmethod total-price ((a-receipt receipt))
  (+ (reduce #'+ (mapcar #'item-total-price (receipt-items a-receipt)))
     (reduce #'+ (mapcar #'discount-amount (receipt-discounts a-receipt)))))

(defmethod add-product-to-receipt ((a-receipt receipt) (a-product product) (a-quantity single-float) (a-price single-float) (a-total-price single-float))
  (push (make-instance 'receipt-item 
                       :product a-product
                       :quantity a-quantity
                       :price a-price
                       :total-price a-total-price) 
        (receipt-items a-receipt)))

(defmethod add-discount ((a-receipt receipt) (a-discount discount))
  (push a-discount (receipt-discounts a-receipt)))
