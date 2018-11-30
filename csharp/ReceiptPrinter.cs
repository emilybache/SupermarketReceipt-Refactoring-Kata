using System;
using System.Collections.Generic;
using System.Globalization;
using System.Text;

namespace supermarket
{
    public class ReceiptPrinter {

    private readonly int _columns;
    private static readonly CultureInfo  _culture = CultureInfo.CreateSpecificCulture("en-GB");


    public ReceiptPrinter(int columns) {
        this._columns = columns;
    }

    public string PrintReceipt(Receipt receipt) {
        StringBuilder result = new StringBuilder();
        foreach (ReceiptItem item in receipt.getItems())
        {
            string price = item.TotalPrice.ToString("F", _culture);
            string quantity = PresentQuantity(item);
            string name = item.Product.Name;
            string unitPrice = item.Price.ToString("F", _culture);

            int whitespaceSize = this._columns - name.Length - price.Length;
            string line = name + GetWhitespace(whitespaceSize) + price + "\n";

            if (item.Quantity != 1) {
                line += "  " + unitPrice + " * " + quantity + "\n";
            }
            result.Append(line);
        }
        foreach (Discount discount in receipt.getDiscounts()) {
            string productPresentation = discount.Product.Name;
            string pricePresentation = discount.DiscountAmount.ToString("F", _culture);
            string description = discount.Description;
            result.Append(description);
            result.Append("(");
            result.Append(productPresentation);
            result.Append(")");
            result.Append(GetWhitespace(this._columns - 3 - productPresentation.Length - description.Length - pricePresentation.Length));
            result.Append("-");
            result.Append(pricePresentation);
            result.Append("\n");
        }

        {
            result.Append("\n");
            string pricePresentation = receipt.getTotalPrice().ToString("F", _culture);
            string total = "Total: ";
            string whitespace = GetWhitespace(this._columns - total.Length - pricePresentation.Length);
            result.Append(total).Append(whitespace).Append(pricePresentation);

            }
            return result.ToString();
    }

    private static string PresentQuantity(ReceiptItem item) {
        return ProductUnit.Each == item.Product.Unit
                ? ((int)item.Quantity).ToString()
                : item.Quantity.ToString("N3", _culture);
    }

    private static string GetWhitespace(int whitespaceSize) {
        StringBuilder whitespace = new StringBuilder();
        for (int i = 0; i < whitespaceSize; i++) {
            whitespace.Append(" ");
        }
        return whitespace.ToString();
    }
}

}
