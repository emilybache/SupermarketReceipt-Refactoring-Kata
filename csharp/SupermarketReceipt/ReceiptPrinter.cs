using System.Globalization;
using System.Text;

namespace SupermarketReceipt
{
    public class ReceiptPrinter
    {
        private static readonly CultureInfo Culture = CultureInfo.CreateSpecificCulture("en-GB");

        private readonly int _columns;


        public ReceiptPrinter(int columns)
        {
            _columns = columns;
        }

        public ReceiptPrinter() : this(40)
        {
        }

        public string PrintReceipt(Receipt receipt)
        {
            var result = new StringBuilder();
            foreach (var item in receipt.GetItems())
            {
                var price = item.TotalPrice.ToString("N2", Culture);
                var quantity = PresentQuantity(item);
                var name = item.Product.Name;
                var unitPrice = item.Price.ToString("N2", Culture);

                var whitespaceSize = _columns - name.Length - price.Length;
                var line = name + GetWhitespace(whitespaceSize) + price + "\n";

                if (item.Quantity != 1) line += "  " + unitPrice + " * " + quantity + "\n";
                result.Append(line);
            }

            foreach (var discount in receipt.GetDiscounts())
            {
                var productPresentation = discount.Product.Name;
                var pricePresentation = discount.DiscountAmount.ToString("N2", Culture);
                var description = discount.Description;
                result.Append(description);
                result.Append("(");
                result.Append(productPresentation);
                result.Append(")");
                result.Append(GetWhitespace(_columns - 3 - productPresentation.Length - description.Length - pricePresentation.Length));
                result.Append("-");
                result.Append(pricePresentation);
                result.Append("\n");
            }

            {
                result.Append("\n");
                var pricePresentation = receipt.GetTotalPrice().ToString("N2", Culture);
                var total = "Total: ";
                var whitespace = GetWhitespace(_columns - total.Length - pricePresentation.Length);
                result.Append(total).Append(whitespace).Append(pricePresentation);
            }
            return result.ToString();
        }

        private static string PresentQuantity(ReceiptItem item)
        {
            return ProductUnit.Each == item.Product.Unit
                ? ((int) item.Quantity).ToString()
                : item.Quantity.ToString("N3", Culture);
        }

        private static string GetWhitespace(int whitespaceSize)
        {
            var whitespace = new StringBuilder();
            for (var i = 0; i < whitespaceSize; i++) whitespace.Append(" ");
            return whitespace.ToString();
        }
    }
}