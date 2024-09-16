;;;; supermarket-receipt.asd

(asdf:defsystem "supermarket-receipt"
  :description ""
  :author "price-queen@supermarket.com"
  :version "1.0.0"
  :depends-on ()
  :pathname "source/"
  :serial T
  :components ((:file "package")
               (:file "model-objects")
               (:file "receipt")
               (:file "catalog")
               (:file "shopping-cart")
               (:file "teller")
               (:file "receipt-printer"))
  :in-order-to ((test-op (test-op "supermarket-receipt/tests"))))


(asdf:defsystem "supermarket-receipt/tests"
	:description "Unit tests for supermarket-receipt"
  :author "price-queen@supermarket.com"
	:version "1.0.0"
	:depends-on ("supermarket-receipt" "parachute" "cl-mock")
	:pathname "tests/"
	:components ((:file "package")
               (:file "fake-catalog")
		           (:file "tests" :depends-on ("package")))
	:perform (test-op (o c) (symbol-call :parachute :test :supermarket-receipt/tests)))
