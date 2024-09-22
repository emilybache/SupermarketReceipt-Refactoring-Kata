;; receipt-printer.lisp

(in-package :supermarket-receipt)

(defclass receipt-printer ()
  ((columns :initarg :columns
            :initform 40
            :type integer
            :accessor printer-columns)))

(defmethod print-receipt ((a-printer receipt-printer) (a-receipt receipt))
  (let ((result ""))
    (loop for item in (reverse (receipt-items a-receipt)) do
      (setf result (concatenate 'string result (print-receipt-item a-printer item))))
    (loop for discount in (reverse (receipt-discounts a-receipt)) do
      (setf result (concatenate 'string result (print-discount a-printer discount))))
    (setf result (format nil "~A~%" result))
    (setf result (concatenate 'string result (present-total a-printer a-receipt)))
    result))

(defmethod print-receipt-item ((a-printer receipt-printer) (an-item receipt-item))
  (let* ((total-price-printed (print-price a-printer (item-total-price an-item)))
         (name (product-name (item-product an-item)))
         (line (format-line-with-whitespace a-printer name total-price-printed)))
    (unless (= 1.0 (item-quantity an-item))
      (setf line (format nil "~A  ~A * ~A~%" line
                         (print-price a-printer (item-price an-item))
                         (print-quantity a-printer an-item))))
    line))

(defmethod format-line-with-whitespace ((a-printer receipt-printer) (a-name string) (a-value string))
  (let* ((line a-name)
         (whitespace-size (- (printer-columns a-printer) (length a-name) (length a-value))))
    (loop for index from 1 to whitespace-size do
          (setf line (concatenate 'string line " ")))
    (setf line (concatenate 'string line a-value))
    (setf line (format nil "~A~%" line))
    line))

(defmethod print-price ((a-printer receipt-printer) (a-price single-float))
  (format nil "~2$" a-price))

(defmethod print-quantity ((a-printer receipt-printer) (an-item receipt-item))
  (if (eq 'each (the-product-unit (item-product an-item)))
      (format nil "~d" (floor (item-quantity an-item)))
      (format nil "~3$" (item-quantity an-item))))

(defmethod print-discount ((a-printer receipt-printer) (a-discount discount))
  (let ((name (format nil "~A(~A)" (discount-description a-discount) (product-name (discounted-product a-discount))))
        (value (print-price a-printer (discount-amount a-discount))))
    (format-line-with-whitespace a-printer name value)))

(defmethod present-total ((a-printer receipt-printer) (a-receipt receipt))
  (let ((name "Total: ")
        (value (print-price a-printer (total-price a-receipt))))
    (format-line-with-whitespace a-printer name value)))
