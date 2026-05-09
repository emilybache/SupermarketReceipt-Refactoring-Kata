using System.Threading.Tasks;
using VerifyXunit;
using Xunit;

namespace SupermarketReceipt.Test
{
    [UsesVerify]
    public class ReceiptPrinterTest
    {
        readonly Product _toothbrush = new Product("toothbrush", ProductUnit.Each);
        readonly Product _apples = new Product("apples", ProductUnit.Kilo);
        Receipt _receipt = new Receipt();

        [Fact]
        public Task OneLineItem()
        {
            _receipt.AddProduct(_toothbrush, 1, 0.99, 0.99);
            return Verifier.Verify(new ReceiptPrinter().PrintReceipt(_receipt));
        }

        [Fact]
        public Task QuantityTwo()
        {
            _receipt.AddProduct(_toothbrush, 2, 0.99, 0.99 * 2);
            return Verifier.Verify(new ReceiptPrinter().PrintReceipt(_receipt));
        }

        [Fact]
        public Task LooseWeight()
        {
            _receipt.AddProduct(_apples, 2.3, 1.99, 1.99 * 2.3);
            return Verifier.Verify(new ReceiptPrinter().PrintReceipt(_receipt));
        }

        [Fact]
        public Task Total()
        {

            _receipt.AddProduct(_toothbrush, 1, 0.99, 2 * 0.99);
            _receipt.AddProduct(_apples, 0.75, 1.99, 1.99 * 0.75);
            return Verifier.Verify(new ReceiptPrinter().PrintReceipt(_receipt));
        }

        [Fact]
        public Task Discounts()
        {
            _receipt.AddDiscount(new Discount(_apples, "3 for 2", 0.99));
            return Verifier.Verify(new ReceiptPrinter().PrintReceipt(_receipt));
        }

        [Fact]
        public Task PrintWholeReceipt()
        {
            _receipt.AddProduct(_toothbrush, 1, 0.99, 0.99);
            _receipt.AddProduct(_toothbrush, 2, 0.99, 2 * 0.99);
            _receipt.AddProduct(_apples, 0.75, 1.99, 1.99 * 0.75);
            _receipt.AddDiscount(new Discount(_toothbrush, "3 for 2", 0.99));
            return Verifier.Verify(new ReceiptPrinter().PrintReceipt(_receipt));
        }
    }
}