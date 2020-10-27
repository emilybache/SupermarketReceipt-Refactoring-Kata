using ApprovalTests;
using ApprovalTests.Reporters;
using Xunit;

namespace SupermarketReceipt.Test
{
    [UseReporter(typeof(DiffReporter))]
    public class ReceiptPrinterTest
    {
        readonly Product _toothbrush = new Product("toothbrush", ProductUnit.Each);
        readonly Product _apples = new Product("apples", ProductUnit.Kilo);
        Receipt _receipt = new Receipt();

        [Fact]
        public void oneLineItem()
        {
            _receipt.AddProduct(_toothbrush, 1, 0.99, 0.99);
            Approvals.Verify(new ReceiptPrinter().PrintReceipt(_receipt));
        }
        
        [Fact]
        public void quantityTwo()
        {
            _receipt.AddProduct(_toothbrush, 2, 0.99, 0.99 * 2);
            Approvals.Verify(new ReceiptPrinter().PrintReceipt(_receipt));
        }
        
        [Fact]
        public void looseWeight()
        {
            _apples.QuantityType = "KG";
            _receipt.AddProduct(_apples, 2.3, 1.99, 1.99 * 2.3);
            Approvals.Verify(new ReceiptPrinter().PrintReceipt(_receipt));
        }

        [Fact]
        public void total()
        {

            _receipt.AddProduct(_toothbrush, 1, 0.99, 2 * 0.99);
            _receipt.AddProduct(_apples, 0.75, 1.99, 1.99 * 0.75);
            Approvals.Verify(new ReceiptPrinter().PrintReceipt(_receipt));
        }

        [Fact]
        public void discounts()
        {
            _receipt.AddDiscount(new Discount(_apples, "3 for 2", 0.99));
            Approvals.Verify(new ReceiptPrinter().PrintReceipt(_receipt));
        }

        [Fact]
        public void printWholeReceipt()
        {
            _receipt.AddProduct(_toothbrush, 1, 0.99, 0.99);
            _receipt.AddProduct(_toothbrush, 2, 0.99, 2 * 0.99);
            _receipt.AddProduct(_apples, 0.75, 1.99, 1.99 * 0.75);
            _receipt.AddDiscount(new Discount(_toothbrush, "3 for 2", 0.99));
            Approvals.Verify(new ReceiptPrinter().PrintReceipt(_receipt));
        }
    }
}