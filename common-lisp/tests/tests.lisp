;;;; tests.lisp

(in-package :supermarket-receipt/tests)

(define-test supermarket-receipt-testsuite)

(define-test "A ten percent discount is not granted for the wrong product."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (apples (make-instance 'product
                                :name "apples"
                                :unit 'kilo))
         (a-price-per-unit 1.0)
         (another-price-per-unit 2.00)
         (number-of-units 2.5)
         (a-discount-in-percent 10.0)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush a-price-per-unit)
    (add-product-to-catalog a-catalog apples another-price-per-unit)
    (add-special-offer a-teller 'ten-percent-discount a-toothbrush a-discount-in-percent)
    (add-item-quantity a-cart apples number-of-units)
    (let* ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = 5.00 (total-price a-receipt)))))

(define-test "A ten percent discount is granted for the right product."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (a-price-per-unit 1.00)
         (number-of-toothbrushes 1.0)
         (a-discount-in-percent 10.0)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush a-price-per-unit)
    (add-special-offer a-teller 'ten-percent-discount a-toothbrush a-discount-in-percent)
    (add-item-quantity a-cart a-toothbrush number-of-toothbrushes)
    (let ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = 0.90 (total-price a-receipt)))))

(define-test "A twenty percent discount is granted for the right product."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (a-price-per-unit 1.00)
         (number-of-toothbrushes 1.0)
         (a-discount-in-percent 20.0)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush a-price-per-unit)
    (add-special-offer a-teller 'ten-percent-discount a-toothbrush a-discount-in-percent)
    (add-item-quantity a-cart a-toothbrush number-of-toothbrushes)
    (let ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = 0.80 (total-price a-receipt)))))

(define-test "A 100 percent discount is granted for the right product."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (a-price-per-unit 1.00)
         (number-of-toothbrushes 1.0)
         (a-discount-in-percent 100.0)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush a-price-per-unit)
    (add-special-offer a-teller 'ten-percent-discount a-toothbrush a-discount-in-percent)
    (add-item-quantity a-cart a-toothbrush number-of-toothbrushes)
    (let ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = 0.00 (total-price a-receipt)))))

(define-test "A 200 percent discount is granted for the right product."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (a-price-per-unit 1.00)
         (number-of-toothbrushes 1.0)
         (a-discount-in-percent 200.0)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush a-price-per-unit)
    (add-special-offer a-teller 'ten-percent-discount a-toothbrush a-discount-in-percent)
    (add-item-quantity a-cart a-toothbrush number-of-toothbrushes)
    (let ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = -1.00 (total-price a-receipt)))))

(define-test "A -100 percent discount is granted for the right product."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (a-price-per-unit 1.00)
         (number-of-toothbrushes 1.0)
         (a-discount-in-percent -100.0)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush a-price-per-unit)
    (add-special-offer a-teller 'ten-percent-discount a-toothbrush a-discount-in-percent)
    (add-item-quantity a-cart a-toothbrush number-of-toothbrushes)
    (let ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = 2.00 (total-price a-receipt)))))

(define-test "Three for two is not granted for two toothbrushes."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (a-price-per-unit 1.00)
         (number-of-toothbrushes 2.0)
         (a-discount-in-percent 10.0)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush a-price-per-unit)
    (add-special-offer a-teller 'three-for-two a-toothbrush a-discount-in-percent)
    (add-item-quantity a-cart a-toothbrush number-of-toothbrushes)
    (let ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = 2.00 (total-price a-receipt)))))

(define-test "Three for two is granted for three toothbrushes."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (a-price-per-unit 1.00)
         (number-of-toothbrushes 3.0)
         (a-discount-in-percent 10.0)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush a-price-per-unit)
    (add-special-offer a-teller 'three-for-two a-toothbrush a-discount-in-percent)
    (add-item-quantity a-cart a-toothbrush number-of-toothbrushes)
    (let ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = 2.00 (total-price a-receipt)))))

(define-test "Three for two is granted for four toothbrushes."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (a-price-per-unit 1.00)
         (number-of-toothbrushes 4.0)
         (a-discount-in-percent 10.0)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush a-price-per-unit)
    (add-special-offer a-teller 'three-for-two a-toothbrush a-discount-in-percent)
    (add-item-quantity a-cart a-toothbrush number-of-toothbrushes)
    (let ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = 3.00 (total-price a-receipt)))))

(define-test "Three for two is granted twice for six toothbrushes."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (a-price-per-unit 1.00)
         (number-of-toothbrushes 6.0)
         (a-discount-in-percent 10.0)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush a-price-per-unit)
    (add-special-offer a-teller 'three-for-two a-toothbrush a-discount-in-percent)
    (add-item-quantity a-cart a-toothbrush number-of-toothbrushes)
    (let ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = 4.00 (total-price a-receipt)))))

(define-test "Two for amount is not granted for wrong product."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (apples (make-instance 'product
                                :name "apples"
                                :unit 'kilo))
         (a-price-per-unit 1.00)
         (another-price-per-unit 2.0)
         (number-of-units 2.5)
         (an-amount 3.50)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush a-price-per-unit)
    (add-product-to-catalog a-catalog apples another-price-per-unit)
    (add-special-offer a-teller 'two-for-amount a-toothbrush an-amount)
    (add-item-quantity a-cart apples number-of-units)
    (let ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = 5.00 (total-price a-receipt)))))

(define-test "Two for amount is not granted for one toothbrush."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (a-price-per-unit 1.00)
         (number-of-toothbrushes 1.0)
         (an-amount 1.50)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush a-price-per-unit)
    (add-special-offer a-teller 'two-for-amount a-toothbrush an-amount)
    (add-item-quantity a-cart a-toothbrush number-of-toothbrushes)
    (let ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = 1.00 (total-price a-receipt)))))

(define-test "Two for amount is granted for two toothbrushes."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (a-price-per-unit 1.00)
         (number-of-toothbrushes 2.0)
         (an-amount 1.50)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush a-price-per-unit)
    (add-special-offer a-teller 'two-for-amount a-toothbrush an-amount)
    (add-item-quantity a-cart a-toothbrush number-of-toothbrushes)
    (let ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = 1.50 (total-price a-receipt)))))

(define-test "Two for amount is granted for three toothbrushes."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (a-price-per-unit 1.00)
         (number-of-toothbrushes 3.0)
         (an-amount 1.50)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush a-price-per-unit)
    (add-special-offer a-teller 'two-for-amount a-toothbrush an-amount)
    (add-item-quantity a-cart a-toothbrush number-of-toothbrushes)
    (let ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = 2.50 (total-price a-receipt)))))

(define-test "Two for amount is granted for four toothbrushes."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (a-price-per-unit 1.00)
         (number-of-toothbrushes 4.0)
         (an-amount 1.50)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush a-price-per-unit)
    (add-special-offer a-teller 'two-for-amount a-toothbrush an-amount)
    (add-item-quantity a-cart a-toothbrush number-of-toothbrushes)
    (let ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = 3.00 (total-price a-receipt)))))

(define-test "Two for negative amount is granted for two toothbrushes."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (a-price-per-unit 1.00)
         (number-of-toothbrushes 2.0)
         (an-amount -1.50)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush a-price-per-unit)
    (add-special-offer a-teller 'two-for-amount a-toothbrush an-amount)
    (add-item-quantity a-cart a-toothbrush number-of-toothbrushes)
    (let ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = -1.50 (total-price a-receipt)))))

(define-test "Two for unwanted amount is granted for two toothbrushes."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (a-price-per-unit 1.00)
         (number-of-toothbrushes 2.0)
         (an-amount 3.00)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush a-price-per-unit)
    (add-special-offer a-teller 'two-for-amount a-toothbrush an-amount)
    (add-item-quantity a-cart a-toothbrush number-of-toothbrushes)
    (let ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = 3.00 (total-price a-receipt)))))

(define-test "Two for amount is not granted for wrong product."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (apples (make-instance 'product
                                :name "apples"
                                :unit 'kilo))
         (a-price-per-unit 1.00)
         (another-price-per-unit 2.0)
         (number-of-units 5.0)
         (an-amount 8.00)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush a-price-per-unit)
    (add-product-to-catalog a-catalog apples another-price-per-unit)
    (add-special-offer a-teller 'five-for-amount a-toothbrush an-amount)
    (add-item-quantity a-cart apples number-of-units)
    (let ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = 10.00 (total-price a-receipt)))))

(define-test "Five for amount is not granted for four toothbrushes."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (a-price-per-unit 1.00)
         (number-of-toothbrushes 4.0)
         (an-amount 4.00)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush a-price-per-unit)
    (add-special-offer a-teller 'five-for-amount a-toothbrush an-amount)
    (add-item-quantity a-cart a-toothbrush number-of-toothbrushes)
    (let ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = 4.00 (total-price a-receipt)))))

(define-test "Five for amount is granted for five toothbrushes."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (a-price-per-unit 1.00)
         (number-of-toothbrushes 5.0)
         (an-amount 4.00)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush a-price-per-unit)
    (add-special-offer a-teller 'five-for-amount a-toothbrush an-amount)
    (add-item-quantity a-cart a-toothbrush number-of-toothbrushes)
    (let ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = 4.00 (total-price a-receipt)))))

(define-test "Five for amount is granted for nine toothbrushes."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (a-price-per-unit 1.00)
         (number-of-toothbrushes 9.0)
         (an-amount 4.00)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush a-price-per-unit)
    (add-special-offer a-teller 'five-for-amount a-toothbrush an-amount)
    (add-item-quantity a-cart a-toothbrush number-of-toothbrushes)
    (let ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = 8.00 (total-price a-receipt)))))

(define-test "Five for amount is granted for ten toothbrushes."
  :parent supermarket-receipt-testsuite
  (let* ((a-catalog (make-instance 'fake-catalog))
         (a-toothbrush (make-instance 'product
                                      :name "toothbrush"
                                      :unit 'each))
         (a-price-per-unit 1.00)
         (number-of-toothbrushes 10.0)
         (an-amount 4.00)
         (a-teller (make-instance 'teller
                                  :catalog a-catalog))
         (a-cart (make-instance 'shopping-cart)))
    (add-product-to-catalog a-catalog a-toothbrush a-price-per-unit)
    (add-special-offer a-teller 'five-for-amount a-toothbrush an-amount)
    (add-item-quantity a-cart a-toothbrush number-of-toothbrushes)
    (let ((a-receipt (checks-out-articles-from a-teller a-cart)))
      (is = 8.00 (total-price a-receipt)))))
