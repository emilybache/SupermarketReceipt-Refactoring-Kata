using NUnit.Framework;

namespace SupermarketReceipt.Test
{
    public class SupermarketNUnitTest
    {
        [TestCase]
        public void TenPercentDiscount()
        {
            // ARRANGE
            SupermarketCatalog catalog = new FakeCatalog();
            var toothbrush = new Product("toothbrush", ProductUnit.Each);
            catalog.AddProduct(toothbrush, 0.99);
            var apples = new Product("apples", ProductUnit.Kilo);
            catalog.AddProduct(apples, 1.99);

            var cart = new ShoppingCart();
            cart.AddItemQuantity(apples, 2.5);

            var teller = new Teller(catalog);
            teller.AddSpecialOffer(SpecialOfferType.TenPercentDiscount, toothbrush, 10.0);

            // ACT
            var receipt = teller.ChecksOutArticlesFrom(cart);

            // ASSERT
            Assert.That(receipt.GetTotalPrice(), Is.EqualTo(4.975));
            Assert.That(receipt.GetDiscounts(), Is.Empty);
            Assert.That(receipt.GetItems().Count, Is.EqualTo(1));
            var receiptItem = receipt.GetItems()[0];
            Assert.That(receiptItem.Product, Is.EqualTo(apples));
            Assert.That(receiptItem.Price, Is.EqualTo(1.99));
            Assert.That(receiptItem.TotalPrice, Is.EqualTo(2.5 * 1.99));
            Assert.That(receiptItem.Quantity, Is.EqualTo(2.5));
        }
    }
}