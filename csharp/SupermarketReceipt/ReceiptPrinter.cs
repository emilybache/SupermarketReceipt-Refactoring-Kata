using System.Globalization;
using System.Text;

namespace SupermarketReceipt
{
    public class ReceiptPrinter
    {

        private readonly int _columns;
        private static readonly CultureInfo Culture = CultureInfo.CreateSpecificCulture("en-GB");



        public ReceiptPrinter(int columns)
        {
            this._columns = columns;
        }

        public ReceiptPrinter() : this(40)
        {
        }

        public string PrintReceipt(Receipt receipt)
        {
            var result = new StringBuilder();
            foreach (var item in receipt.GetItems())
            {
                var price = item.TotalPrice.ToString("F", Culture);
                var quantity = PresentQuantity(item);
                var name = item.Product.Name;
                var unitPrice = item.Price.ToString("F", Culture);

                var whitespaceSize = this._columns - name.Length - price.Length;
                var line = name + GetWhitespace(whitespaceSize) + price + "\n";

                if (item.Quantity != 1)
                {
                    line += "  " + unitPrice + " * " + quantity + "\n";
                }
                result.Append(line);
            }
            foreach (Discount discount in receipt.GetDiscounts())
            {
                var productPresentation = discount.Product.Name;
                var pricePresentation = discount.DiscountAmount.ToString("F", Culture);
                var description = discount.Description;
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
                var pricePresentation = receipt.GetTotalPrice().ToString("F", Culture);
                var total = "Total: ";
                var whitespace = GetWhitespace(this._columns - total.Length - pricePresentation.Length);
                result.Append(total).Append(whitespace).Append(pricePresentation);

            }
            return result.ToString();
        }

        private static string PresentQuantity(ReceiptItem item)
        {
            return ProductUnit.Each == item.Product.Unit
                    ? ((int)item.Quantity).ToString()
                    : item.Quantity.ToString("N3", Culture);
        }

        private static string GetWhitespace(int whitespaceSize)
        {
            var whitespace = new StringBuilder();
            for (var i = 0; i < whitespaceSize; i++)
            {
                whitespace.Append(" ");
            }
            return whitespace.ToString();
        }
    }

}
