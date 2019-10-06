@testable import SupermarketReceipt

public class ReceiptPrinterTest {

    var toothbrush = Product(name: "toothbrush", unit: ProductUnit.Each)
    var apples = Product(name: "apples", unit: ProductUnit.Kilo)
    var receipt = Receipt()

    public func oneLineItem() {
        receipt.addProduct(p: toothbrush, quantity: 1, price: 0.99, totalPrice: 0.99)
        let result = ReceiptPrinter(columns: 40).printReceipt(receipt: receipt)
    }

    public func quantityTwo() {
        receipt.addProduct(p: toothbrush, quantity: 2, price: 0.99,totalPrice: 0.99 * 2)
        let result = ReceiptPrinter(columns: 40).printReceipt(receipt: receipt)
    }

    public func looseWeight() {
        receipt.addProduct(p: apples, quantity: 2.3, price: 1.99,totalPrice: 1.99 * 2.3)
        let result = ReceiptPrinter(columns: 40).printReceipt(receipt: receipt)
    }

    public func total() {
        receipt.addProduct(p: toothbrush, quantity: 1, price: 0.99, totalPrice: 2*0.99)
        receipt.addProduct(p: apples, quantity: 0.75, price: 1.99, totalPrice: 1.99*0.75)
        let result = ReceiptPrinter(columns: 40).printReceipt(receipt: receipt)
    }

    public func discounts() {
        receipt.addDiscount(discount: Discount(description: "3 for 2", discountAmount: 0.99, product: apples))
        let result = ReceiptPrinter(columns: 40).printReceipt(receipt: receipt)
    }

    public func printWholeReceipt() {
        receipt.addProduct(p: toothbrush, quantity: 1, price: 0.99, totalPrice: 0.99)
        receipt.addProduct(p: toothbrush, quantity: 2, price: 0.99, totalPrice: 2*0.99)
        receipt.addProduct(p: apples, quantity: 0.75, price: 1.99, totalPrice: 1.99*0.75)
        receipt.addDiscount(discount: Discount(description: "3 for 2", discountAmount: 0.99, product: toothbrush))
        let result = ReceiptPrinter(columns: 40).printReceipt(receipt: receipt)
    }

}

