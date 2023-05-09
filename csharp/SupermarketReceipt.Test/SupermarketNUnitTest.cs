using System.Collections.Generic;
using NUnit;
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
            Assert.AreEqual(4.975, receipt.GetTotalPrice());
            CollectionAssert.IsEmpty(receipt.GetDiscounts());
            Assert.AreEqual(1, receipt.GetItems().Count);
            var receiptItem = receipt.GetItems()[0];
            Assert.AreEqual(apples, receiptItem.Product);
            Assert.AreEqual(1.99, receiptItem.Price);
            Assert.AreEqual(2.5 * 1.99, receiptItem.TotalPrice);
            Assert.AreEqual(2.5, receiptItem.Quantity);
        }
    }
}