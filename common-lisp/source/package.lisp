;;;; package.lisp

(defpackage :supermarket-receipt
  (:use :common-lisp)
  (:export 
   :product
   :product-name
   :special-offer-type
   :product-unit
   :each
   :kilo
   :shopping-cart
   :add-item-quantity
   :teller
   :add-special-offer
   :checks-out-articles-from
   :discount
   :receipt
   :receipt-items
   :receipt-discounts
   :add-product-to-receipt
   :add-discount
   :ten-percent-discount
   :three-for-two
   :two-for-amount
   :five-for-amount
   :item-product
   :total-price
   :item-quantity
   :item-price
   :item-total-price
   :supermarket-catalog
   :catalog-products
   :catalog-prices
   :add-product-to-catalog
   :unit-price
   :receipt-printer
   :print-receipt))
