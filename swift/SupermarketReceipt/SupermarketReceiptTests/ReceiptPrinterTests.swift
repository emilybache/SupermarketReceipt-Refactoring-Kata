import XCTest

@testable import SupermarketReceipt

public class ReceiptPrinterTest: XCTestCase {

    var toothbrush = Product(name: "toothbrush", unit: ProductUnit.Each)
    var apples = Product(name: "apples", unit: ProductUnit.Kilo)
    var receipt = Receipt()

    public func test_oneLineItem() {
        receipt.addProduct(p: toothbrush, quantity: 1, price: 0.99, totalPrice: 0.99)
        let result = ReceiptPrinter(columns: 40).printReceipt(receipt: receipt)
        let expected = """
toothbrush                          0.99
  1.00 * 0

Total:                              0.99
"""
        XCTAssertEqual(expected, result)
    }

    public func test_quantityTwo() {
        receipt.addProduct(p: toothbrush, quantity: 2, price: 0.99,totalPrice: 0.99 * 2)
        let result = ReceiptPrinter(columns: 40).printReceipt(receipt: receipt)
        let expected = """
toothbrush                          0.99
  2.00 * 1

Total:                              0.99
"""
        XCTAssertEqual(expected, result)
    }

    public func test_looseWeight() {
        receipt.addProduct(p: apples, quantity: 2.3, price: 1.99,totalPrice: 1.99 * 2.3)
        let result = ReceiptPrinter(columns: 40).printReceipt(receipt: receipt)
        let expected = """
apples                              1.99
  2.30 * 4.577

Total:                              1.99
"""
        XCTAssertEqual(expected, result)
    }

    public func test_total() {
        receipt.addProduct(p: toothbrush, quantity: 1, price: 0.99, totalPrice: 2*0.99)
        receipt.addProduct(p: apples, quantity: 0.75, price: 1.99, totalPrice: 1.99*0.75)
        let result = ReceiptPrinter(columns: 40).printReceipt(receipt: receipt)
        let expected = """
toothbrush                          0.99
  1.00 * 1
apples                              1.99
  0.75 * 1.492

Total:                              2.98
"""
        XCTAssertEqual(expected, result)
    }

    public func test_discounts() {
        receipt.addDiscount(discount: Discount(description: "3 for 2", discountAmount: 0.99, product: apples))
        let result = ReceiptPrinter(columns: 40).printReceipt(receipt: receipt)
        let expected = """
3 for 2(apples)                    -0.99

Total:                             -0.99
"""
        XCTAssertEqual(expected, result)
    }

    public func test_printWholeReceipt() {
        receipt.addProduct(p: toothbrush, quantity: 1, price: 0.99, totalPrice: 0.99)
        receipt.addProduct(p: toothbrush, quantity: 2, price: 0.99, totalPrice: 2*0.99)
        receipt.addProduct(p: apples, quantity: 0.75, price: 1.99, totalPrice: 1.99*0.75)
        receipt.addDiscount(discount: Discount(description: "3 for 2", discountAmount: 0.99, product: toothbrush))
        let result = ReceiptPrinter(columns: 40).printReceipt(receipt: receipt)
        let expected = """
toothbrush                          0.99
  1.00 * 0
toothbrush                          0.99
  2.00 * 1
apples                              1.99
  0.75 * 1.492
3 for 2(toothbrush)                -0.99

Total:                              2.98
"""
        XCTAssertEqual(expected, result)
    }

}

