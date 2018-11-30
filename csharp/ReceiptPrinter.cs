using System;
using System.Collections.Generic;
using System.Globalization;
using System.Text;

namespace supermarket
{
    public class ReceiptPrinter {

    private int columns;
    private static CultureInfo  culture = CultureInfo.CreateSpecificCulture("en-GB");


    public ReceiptPrinter(int columns) {
        this.columns = columns;
    }

    public string printReceipt(Receipt receipt) {
        StringBuilder result = new StringBuilder();
        foreach (ReceiptItem item in receipt.getItems())
        {
            string price = item.getTotalPrice().ToString("F", culture);
            string quantity = presentQuantity(item);
            string name = item.getProduct().getName();
            string unitPrice = item.getPrice().ToString("F", culture);

            int whitespaceSize = this.columns - name.Length - price.Length;
            string line = name + getWhitespace(whitespaceSize) + price + "\n";

            if (item.getQuantity() != 1) {
                line += "  " + unitPrice + " * " + quantity + "\n";
            }
            result.Append(line);
        }
        foreach (Discount discount in receipt.getDiscounts()) {
            string productPresentation = discount.getProduct().getName();
            string pricePresentation = discount.getDiscountAmount().ToString("F", culture);
            string description = discount.getDescription();
            result.Append(description);
            result.Append("(");
            result.Append(productPresentation);
            result.Append(")");
            result.Append(getWhitespace(this.columns - 3 - productPresentation.Length - description.Length - pricePresentation.Length));
            result.Append("-");
            result.Append(pricePresentation);
            result.Append("\n");
        }

        {
            result.Append("\n");
            string pricePresentation = receipt.getTotalPrice().ToString("F", culture);
            string total = "Total: ";
            string whitespace = getWhitespace(this.columns - total.Length - pricePresentation.Length);
            result.Append(total).Append(whitespace).Append(pricePresentation);

            }
            return result.ToString();
    }

    private static string presentQuantity(ReceiptItem item) {
        return ProductUnit.Each == item.getProduct().getUnit()
                ? ((int)item.getQuantity()).ToString()
                : item.getQuantity().ToString("N3", culture);
    }

    private static string getWhitespace(int whitespaceSize) {
        StringBuilder whitespace = new StringBuilder();
        for (int i = 0; i < whitespaceSize; i++) {
            whitespace.Append(" ");
        }
        return whitespace.ToString();
    }
}

}
