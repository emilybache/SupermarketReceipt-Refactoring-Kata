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
        (total-price-for-toothbrushes 0.99)
        (a-receipt (make-instance 'receipt))
        (a-printer (make-instance 'receipt-printer
                                  :columns 40)))
    (add-product-to-receipt a-receipt a-toothbrush quantity-of-toothbrushes price-for-toothbrush total-price-for-toothbrushes)
    (let ((printed-receipt (print-receipt a-printer a-receipt)))
      (is string= "toothbrush                          0.99

Total:                              0.99
" printed-receipt))))

(define-test "Quantity two"
  :parent receipt-printer-testsuite
  (let ((a-toothbrush (make-instance 'product
                                     :name "toothbrush"
                                     :unit 'supermarket-receipt::each))
        (price-for-toothbrush 0.99)
        (quantity-of-toothbrushes 2.0)
        (total-price-for-toothbrushes 1.98)
        (a-receipt (make-instance 'receipt))
        (a-printer (make-instance 'receipt-printer
                                  :columns 40)))
    (add-product-to-receipt a-receipt a-toothbrush quantity-of-toothbrushes price-for-toothbrush total-price-for-toothbrushes)
    (let ((printed-receipt (print-receipt a-printer a-receipt)))
      (is string= "toothbrush                          1.98
  0.99 * 2

Total:                              1.98
" printed-receipt))))

(define-test "Loose weight"
  :parent receipt-printer-testsuite
  (let ((apples (make-instance 'product
                               :name "apples"
                               :unit 'supermarket-receipt::kilo))
        (price-for-apples 1.99)
        (quantity-of-apples 2.3)
        (total-price-for-apples (* 1.99 2.3))
        (a-receipt (make-instance 'receipt))
        (a-printer (make-instance 'receipt-printer
                                  :columns 40)))
    (add-product-to-receipt a-receipt apples quantity-of-apples price-for-apples total-price-for-apples)
    (let ((printed-receipt (print-receipt a-printer a-receipt)))
      (is string= "apples                              4.58
  1.99 * 2.300

Total:                              4.58
" printed-receipt))))

(define-test "Total"
  :parent receipt-printer-testsuite
  (let ((a-toothbrush (make-instance 'product
                                     :name "toothbrush"
                                     :unit 'supermarket-receipt::each))
        (price-for-toothbrush 0.99)
        (quantity-of-toothbrushes 1.0)
        (total-price-for-toothbrushes (* 0.99 2.0))
        (apples (make-instance 'product
                               :name "apples"
                               :unit 'supermarket-receipt::kilo))
        (price-for-apples 1.99)
        (quantity-of-apples 0.75)
        (total-price-for-apples (* 1.99 0.75))
        (a-receipt (make-instance 'receipt))
        (a-printer (make-instance 'receipt-printer
                                  :columns 40)))
    (add-product-to-receipt a-receipt a-toothbrush quantity-of-toothbrushes price-for-toothbrush total-price-for-toothbrushes)
    (add-product-to-receipt a-receipt apples quantity-of-apples price-for-apples total-price-for-apples)
    (let ((printed-receipt (print-receipt a-printer a-receipt)))
      (is string= "toothbrush                          1.98
apples                              1.49
  1.99 * 0.750

Total:                              3.47
" printed-receipt))))

(define-test "Discounts"
  :parent receipt-printer-testsuite
  (let* ((apples (make-instance 'product
                                :name "apples"
                                :unit 'supermarket-receipt::kilo))
         (a-discount (make-instance 'discount
                                    :product apples
                                    :description "3 for 2"
                                    :amount -0.99))
         (a-receipt (make-instance 'receipt))
         (a-printer (make-instance 'receipt-printer
                                   :columns 40)))
    (add-discount a-receipt a-discount)
    (let ((printed-receipt (print-receipt a-printer a-receipt)))
      (is string= "3 for 2(apples)                    -0.99

Total:                             -0.99
" printed-receipt))))

(define-test "Print whole receipt"
  :parent receipt-printer-testsuite
  (let* ((a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'supermarket-receipt::each))
         (price-for-toothbrush 0.99)
         (quantity-of-toothbrushes 1.0)
         (total-price-for-toothbrushes 0.99)
         (apples (make-instance 'product
                                :name "apples"
                                :unit 'supermarket-receipt::kilo))
         (price-for-apples 1.99)
         (quantity-of-apples 0.75)
         (total-price-for-apples (* 1.99 0.75))
         (a-discount (make-instance 'discount
                                    :product a-toothbrush
                                    :description "3 for 2"
                                    :amount -0.99))
         (a-receipt (make-instance 'receipt))
         (a-printer (make-instance 'receipt-printer
                                   :columns 40)))
    (add-product-to-receipt a-receipt a-toothbrush quantity-of-toothbrushes price-for-toothbrush total-price-for-toothbrushes)
    (add-product-to-receipt a-receipt a-toothbrush 2.0 price-for-toothbrush (* 2.0 0.99))
    (add-product-to-receipt a-receipt apples quantity-of-apples price-for-apples total-price-for-apples)
    (add-discount a-receipt a-discount)
    (let ((printed-receipt (print-receipt a-printer a-receipt)))
      (is string= "toothbrush                          0.99
toothbrush                          1.98
  0.99 * 2
apples                              1.49
  1.99 * 0.750
3 for 2(toothbrush)                -0.99

Total:                              3.47
" printed-receipt))))
