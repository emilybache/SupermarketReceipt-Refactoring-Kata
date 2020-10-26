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
                string receiptItem = PrintReceiptItem(item);
                result.Append(receiptItem);
                
            }

            foreach (var discount in receipt.GetDiscounts())
            {
                string discountPresentation = PrintDiscount(discount);
                result.Append(discountPresentation);
            }

            {
                result.Append("\n");
                result.Append(PrintTotal(receipt));
            }
            return result.ToString();
        }

        private string PrintTotal(Receipt receipt)
        {
            string name = "Total: ";
            string value = PrintPrice(receipt.GetTotalPrice());
            return FormatLineWithWhitespace(name, value);
        }

        private string PrintDiscount(Discount discount)
        {
            string name = discount.Description + "(" + discount.Product.Name + ")";
            string value = PrintPrice(discount.DiscountAmount);

            return FormatLineWithWhitespace(name, value);
        }

        private string PrintReceiptItem(ReceiptItem item)
        {
            string totalPrice = PrintPrice(item.TotalPrice);
            string name = item.Product.Name;
            string line = FormatLineWithWhitespace(name, totalPrice);
            if (item.Quantity != 1)
            {
                line += "  " + PrintPrice(item.Price) + " * " + PrintQuantity(item) + "\n";
            }

            return line;
        }
        

        private string FormatLineWithWhitespace(string name, string value)
        {
            var line = new StringBuilder();
            line.Append(name);
            int whitespaceSize = this._columns - name.Length - value.Length;
            for (int i = 0; i < whitespaceSize; i++) {
                line.Append(" ");
            }
            line.Append(value);
            line.Append('\n');
            return line.ToString();
        }

        private string PrintPrice(double price)
        {
            return price.ToString("N2", Culture);
        }

        private static string PrintQuantity(ReceiptItem item)
        {
            return ProductUnit.Each == item.Product.Unit
                ? ((int) item.Quantity).ToString()
                : item.Quantity.ToString("N3", Culture);
        }
        
    }
}