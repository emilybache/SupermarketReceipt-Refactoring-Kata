public class ReceiptPrinter {

    private var columns: Int = 40

    public init(columns: Int) {
        self.columns = columns
    }

    public func printReceipt(receipt: Receipt) -> String {
        var result = ""
        for item in receipt.items {
            var price = String(format: "%.2f", item.totalPrice)
            var quantity = ReceiptPrinter.presentQuantity(item: item)
            var name = item.product.name
            var unitPrice = String(format :"%.2f", item.price)

            var whitespaceSize = self.columns - name.count - price.count
            var line = name + ReceiptPrinter.getWhitespace(whitespaceSize: whitespaceSize) + price + "\n"

            if (item.quantity != 1) {
                line += "  " + unitPrice + " * " + quantity + "\n"
            }
            result.append(line)
        }
        for discount in receipt.discounts {
            var productPresentation = discount.product.name
            var pricePresentation = String(format: "%.2f", discount.discountAmount)
            var description = discount.description
            result.append(description)
            result.append("(")
            result.append(productPresentation)
            result.append(")")
            result.append(ReceiptPrinter.getWhitespace(whitespaceSize: self.columns - 3 - productPresentation.count - description.count - pricePresentation.count))
            result.append("-")
            result.append(pricePresentation)
            result.append("\n")
        }
        result.append("\n")
        var pricePresentation = String(format: "%.2f", Double(receipt.getTotalPrice()))
        var total = "Total: "
        var whitespace = ReceiptPrinter.getWhitespace(whitespaceSize: self.columns - total.count - pricePresentation.count)
        result.append(total)
        result.append(whitespace)
        result.append(pricePresentation)
        return result
    }

    private static func presentQuantity(item: ReceiptItem ) -> String {
        return ProductUnit.Each == item.product.unit
            ? String(format: "%x", Int(item.quantity))
            : String(format: "%.3f", item.quantity)
    }

    private static func getWhitespace(whitespaceSize: Int) -> String {
        var whitespace = ""
        for i in 0..<whitespaceSize {
            whitespace.append(" ")
        }
        return whitespace
    }
}
