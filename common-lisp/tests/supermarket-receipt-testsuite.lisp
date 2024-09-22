;;;; supermarket-receipt-testsuite.lisp

(in-package :common-lisp-user)

(defpackage :supermarket-receipt/testsuite
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

(in-package :supermarket-receipt/testsuite)

(define-test supermarket-receipt-testsuite)

(define-test "An empty cart costs nothing."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (price-for-toothbrush 1.00)
         (apples (make-instance 'product
                                :name "apples"
                                :unit 'kilo))
         (price-for-apples 2.00)
         (expected-price 0.00)
         (a-teller (make-instance 'teller
                     :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush price-for-toothbrush)
    (add-product-to-catalog a-catalog apples price-for-apples)
    (let ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = expected-price (total-price a-receipt)))))

(define-test "A normal item."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (price-for-toothbrush  1.00)
         (quantity-of-toothbrushes 1.0)
         (expected-price 1.00)
         (a-teller (make-instance 'teller
                     :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush price-for-toothbrush)
    (add-item-quantity a-cart a-toothbrush quantity-of-toothbrushes)
    (let ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = expected-price (total-price a-receipt)))))

(define-test "Two normal items."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (price-for-toothbrush 1.00)
         (quantity-of-toothbrushes 2.0)
         (expected-price 2.00)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush price-for-toothbrush)
    (add-item-quantity a-cart a-toothbrush quantity-of-toothbrushes)
    (let ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = expected-price (total-price a-receipt)))))

(define-test "Loose weight product."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (apples (make-instance 'product
                                :name "apples"
                                :unit 'kilo))
         (price-for-apples 2.00)
         (quantity-of-apples 0.5)
         (expected-price 1.00)
         (a-teller (make-instance 'teller
                     :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog apples price-for-apples)
    (add-item-quantity a-cart apples quantity-of-apples)
    (let ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = expected-price (total-price a-receipt)))))

(define-test "A ten percent discount is not granted for the wrong product."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (price-for-toothbrush 1.00)
         (discount-for-toothbrush 10.0)
         (apples (make-instance 'product
                                :name "apples"
                                :unit 'kilo))
         (price-for-apples 2.00)
         (quantity-of-apples 2.5)
         (expected-price 5.00)
         (a-teller (make-instance 'teller
                     :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush price-for-toothbrush)
    (add-product-to-catalog a-catalog apples price-for-apples)
    (add-special-offer a-teller 'ten-percent-discount a-toothbrush discount-for-toothbrush)
    (add-item-quantity a-cart apples quantity-of-apples)
    (let ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = expected-price (total-price a-receipt)))))

(define-test "A ten percent discount is granted for the right product."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (price-for-toothbrush 1.00)
         (number-of-toothbrushes 1.0)
         (discount-for-toothbrush 10.0)
         (expected-price 0.90)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush price-for-toothbrush)
    (add-item-quantity a-cart a-toothbrush number-of-toothbrushes)
    (add-special-offer a-teller 'ten-percent-discount a-toothbrush discount-for-toothbrush)
    (let ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = expected-price (total-price a-receipt)))))

(define-test "A three for two discount is not granted for two of the right product."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (price-for-toothbrush 1.00)
         (number-of-toothbrushes 2.0)
         (discount-for-toothbrush 10.0)
         (expected-price 2.00)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush price-for-toothbrush)
    (add-item-quantity a-cart a-toothbrush number-of-toothbrushes)
    (add-special-offer a-teller 'three-for-two a-toothbrush discount-for-toothbrush)
    (let ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = expected-price (total-price a-receipt)))))

(define-test "A three for two discount is granted for three of the right product."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (price-for-toothbrush 1.00)
         (number-of-toothbrushes 3.0)
         (discount-for-toothbrush 10.0)
         (expected-price 2.00)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush price-for-toothbrush)
    (add-item-quantity a-cart a-toothbrush number-of-toothbrushes)
    (add-special-offer a-teller 'three-for-two a-toothbrush discount-for-toothbrush)
    (let ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = expected-price (total-price a-receipt)))))

(define-test "A three for two discount is granted for five of the right product."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (price-for-toothbrush 1.00)
         (number-of-toothbrushes 5.0)
         (discount-for-toothbrush 10.0)
         (expected-price 4.00)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush price-for-toothbrush)
    (add-item-quantity a-cart a-toothbrush number-of-toothbrushes)
    (add-special-offer a-teller 'three-for-two a-toothbrush discount-for-toothbrush)
    (let ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = expected-price (total-price a-receipt)))))

(define-test "A two for amount discount is not granted for one of the right product."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (price-for-toothbrush 1.00)
         (number-of-toothbrushes 1.0)
         (amount 1.50)
         (expected-price 1.00)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush price-for-toothbrush)
    (add-item-quantity a-cart a-toothbrush number-of-toothbrushes)
    (add-special-offer a-teller 'two-for-amount a-toothbrush amount)
    (let ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = expected-price (total-price a-receipt)))))

(define-test "A two for amount discount is granted for two of the right product."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (price-for-toothbrush 1.00)
         (number-of-toothbrushes 2.0)
         (amount 1.50)
         (expected-price 1.50)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush price-for-toothbrush)
    (add-item-quantity a-cart a-toothbrush number-of-toothbrushes)
    (add-special-offer a-teller 'two-for-amount a-toothbrush amount)
    (let ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = expected-price (total-price a-receipt)))))

(define-test "A five for amount discount is not granted for four of the right product."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (price-for-toothbrush 1.00)
         (number-of-toothbrushes 4.0)
         (amount 4.00)
         (expected-price 4.00)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush price-for-toothbrush)
    (add-item-quantity a-cart a-toothbrush number-of-toothbrushes)
    (add-special-offer a-teller 'five-for-amount a-toothbrush amount)
    (let ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = expected-price (total-price a-receipt)))))

(define-test "A five for amount discount is granted for five of the right product."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (price-for-toothbrush 1.00)
         (number-of-toothbrushes 5.0)
         (amount 4.00)
         (expected-price 4.00)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush price-for-toothbrush)
    (add-item-quantity a-cart a-toothbrush number-of-toothbrushes)
    (add-special-offer a-teller 'five-for-amount a-toothbrush amount)
    (let ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = expected-price (total-price a-receipt)))))

(define-test "A five for amount discount is granted for six of the right product."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (price-for-toothbrush 1.00)
         (number-of-toothbrushes 6.0)
         (amount 4.00)
         (expected-price 5.00)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush price-for-toothbrush)
    (add-item-quantity a-cart a-toothbrush number-of-toothbrushes)
    (add-special-offer a-teller 'five-for-amount a-toothbrush amount)
    (let ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = expected-price (total-price a-receipt)))))

(define-test "A five for amount discount is granted thrice for sixteen of the right product."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (price-for-toothbrush 1.00)
         (number-of-toothbrushes 16.0)
         (amount 4.00)
         (expected-price 13.00)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush price-for-toothbrush)
    (add-item-quantity a-cart a-toothbrush number-of-toothbrushes)
    (add-special-offer a-teller 'five-for-amount a-toothbrush amount)
    (let ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = expected-price (total-price a-receipt)))))
