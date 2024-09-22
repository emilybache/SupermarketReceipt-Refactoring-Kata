;;;; receipt-printer-testsuite.lisp

(in-package :common-lisp-user)

(defpackage :supermarket-receipt/receipt-printer-testsuite
  (:use :common-lisp
        :supermarket-receipt
        :supermarket-receipt/fake-catalog)
  (:import-from :parachute
                :define-test
                :is)
  (:import-from :cl-mock
                :with-mocks
                :answer
                :call-previous
                :invocations))

(in-package :supermarket-receipt/receipt-printer-testsuite)

(define-test receipt-printer-testsuite)

(define-test "One line item"
  :parent receipt-printer-testsuite
  (let ((a-toothbrush (make-instance 'product
                                     :name "toothbrush"
                                     :unit 'supermarket-receipt::each))
        (price-for-toothbrush 0.99)
        (quantity-of-toothbrushes 1.0)
        (receipt-price 0.99)
        (a-receipt (make-instance 'receipt))
        (a-printer (make-instance 'receipt-printer
                                  :columns 40)))
    (add-product-to-receipt a-receipt a-toothbrush quantity-of-toothbrushes price-for-toothbrush receipt-price)
    (let ((printed-receipt (print-receipt a-printer a-receipt)))
      (is string= "toothbrush                          0.99

Total:                              0.99
" printed-receipt))))

(define-test "Quantity two"
  :parent receipt-printer-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'supermarket-receipt::each))
         (price-for-toothbrush  0.99)
         (quantity-of-toothbrushes 2.0)
         (a-teller (make-instance 'teller
                     :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart))
         (a-printer (make-instance 'receipt-printer
                                   :columns 40)))
    (add-product-to-catalog a-catalog a-toothbrush price-for-toothbrush)
    (add-item-quantity a-cart a-toothbrush quantity-of-toothbrushes)
    (let* ((a-receipt (checks-out-articles-from a-teller a-cart))
           (printed-receipt (print-receipt a-printer a-receipt)))
      (is string= "toothbrush                          1.98
  0.99 * 2

Total:                              1.98
" printed-receipt))))

(define-test "Loose weight"
  :parent receipt-printer-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (price-for-toothbrush  0.99)
         (quantity-of-toothbrushes 1.0)
         (expected-price 0.99)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart))
         (a-printer (make-instance 'receipt-printer
                                   :columns 40)))
    (add-product-to-catalog a-catalog a-toothbrush price-for-toothbrush)
    (add-item-quantity a-cart a-toothbrush quantity-of-toothbrushes)
    (let* ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = expected-price (total-price a-receipt)))))

(define-test "Total"
  :parent receipt-printer-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (price-for-toothbrush  0.99)
         (quantity-of-toothbrushes 1.0)
         (expected-price 0.99)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart))
         (a-printer (make-instance 'receipt-printer
                                   :columns 40)))
    (add-product-to-catalog a-catalog a-toothbrush price-for-toothbrush)
    (add-item-quantity a-cart a-toothbrush quantity-of-toothbrushes)
    (let* ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = expected-price (total-price a-receipt)))))

(define-test "Discounts"
  :parent receipt-printer-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (price-for-toothbrush  0.99)
         (quantity-of-toothbrushes 1.0)
         (expected-price 0.99)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart))
         (a-printer (make-instance 'receipt-printer
                                   :columns 40)))
    (add-product-to-catalog a-catalog a-toothbrush price-for-toothbrush)
    (add-item-quantity a-cart a-toothbrush quantity-of-toothbrushes)
    (let* ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = expected-price (total-price a-receipt)))))

(define-test "Print whole receipt"
  :parent receipt-printer-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (price-for-toothbrush  0.99)
         (quantity-of-toothbrushes 1.0)
         (expected-price 0.99)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart))
         (a-printer (make-instance 'receipt-printer
                                   :columns 40)))
    (add-product-to-catalog a-catalog a-toothbrush price-for-toothbrush)
    (add-item-quantity a-cart a-toothbrush quantity-of-toothbrushes)
    (let* ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = expected-price (total-price a-receipt)))))

