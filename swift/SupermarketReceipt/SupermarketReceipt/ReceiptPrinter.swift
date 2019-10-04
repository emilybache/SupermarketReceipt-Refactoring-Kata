public class ReceiptPrinter {

    private var columns: Int

    public init() {
        super.init(40)
    }

    public init(columns: Int) {
        self.columns = columns
    }

    public func printReceipt(receipt: Receipt) -> String {
        var result = ""
        for item in receipt.items {
            var price = String.format(Locale.UK, "%.2f", item.totalPrice)
            var quantity = self.presentQuantity(item: item)
            var name = item.product.name
            var unitPrice = String.format(Locale.UK, "%.2f", item.price)

            var whitespaceSize = self.columns - name.length() - price.length()
            var line = name + getWhitespace(whitespaceSize) + price + "\n"

            if (item.quantity != 1) {
                line += "  " + unitPrice + " * " + quantity + "\n"
            }
            result.append(line)
        }
        for discount in receipt.discounts {
            var productPresentation = discount.product.name
            var pricePresentation = String.format(Locale.UK, "%.2f", discount.discountAmount)
            var description = discount.description
            result.append(description)
            result.append("(")
            result.append(productPresentation)
            result.append(")")
            result.append(getWhitespace(self.columns - 3 - productPresentation.length() - description.length() - pricePresentation.length()))
            result.append("-")
            result.append(pricePresentation)
            result.append("\n")
        }
        result.append("\n")
        var pricePresentation = String.format(Locale.UK, "%.2f", Double(receipt.getTotalPrice()))
        var total = "Total: "
        var whitespace = getWhitespace(self.columns - total.length() - pricePresentation.length())
        result.append(total).append(whitespace).append(pricePresentation)
        return result
    }

    private static func presentQuantity(item: ReceiptItem ) -> String {
        return ProductUnit.Each.equals(item.product.unit)
                ? String.format("%x", Int(item.getQuantity()))
                : String.format(Locale.UK, "%.3f", item.getQuantity())
    }

    private static func getWhitespace(whitespaceSize: Int) -> String {
        var whitespace = ""
        for i in 0..<whitespaceSize {
            whitespace.append(" ")
        }
        return whitespace
    }
}
