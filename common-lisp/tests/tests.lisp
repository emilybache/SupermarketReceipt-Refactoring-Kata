;;;; tests.lisp

(in-package :supermarket-receipt/tests)

(define-test supermarket-receipt-testsuite)

(define-test "Ten percent discount"
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                         :name "toothbrush"
                         :unit 'each))
         (apples (make-instance 'product
                   :name "apples"
                   :unit 'kilo))
         (a-teller (make-instance 'teller
                     :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush 0.99)
    (add-product-to-catalog a-catalog apples 1.99)
    (add-special-offer a-teller 'ten-percent-discount a-toothbrush 10.0)
    (add-item-quantity a-cart apples 2.5)
    (let* ((a-receipt (checks-out-articles-from a-teller a-cart))
           (total-receipt-price (total-price a-receipt))
           (discounts (receipt-discounts a-receipt))
           (items (receipt-items a-receipt))
           (first-receipt-item (first items))
           (receipt-item-product (item-product first-receipt-item))
           (the-price (item-price first-receipt-item))
           (the-total-price (item-total-price first-receipt-item))
           (the-quantity (item-quantity first-receipt-item)))
      (is = 4.975 total-receipt-price)
      (is equal nil discounts)
      (is = 1 (length items))
      (is equal apples receipt-item-product)
      (is = 1.99 the-price)
      (is = (* 2.5 1.99) the-total-price)
      (is = 2.5 the-quantity))))
