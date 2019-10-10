public class Receipt {
    var items = [ReceiptItem]()
    var discounts = [Discount]()

    public func getTotalPrice() -> Double {
        var total: Double = 0.0
        for item in self.items {
            total += item.totalPrice
        }
        for discount in self.discounts {
            total -= discount.discountAmount
        }
        return total
    }

    public func addProduct(p: Product, quantity: Double, price: Double, totalPrice: Double) {
        self.items.append(ReceiptItem(product: p, price: price, totalPrice: totalPrice, quantity: quantity))
    }

    public func addDiscount(discount: Discount) {
        self.discounts.append(discount)
    }
}
