using System.Collections.Generic;
using Xunit;

namespace SupermarketReceipt.Test
{
    public class SupermarketXUnitTest
    {
        [Fact]
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
            Assert.Equal(4.975, receipt.GetTotalPrice());
            Assert.Equal([], receipt.GetDiscounts());
            var receiptItem = Assert.Single(receipt.GetItems());
            Assert.Equal(apples, receiptItem.Product);
            Assert.Equal(1.99, receiptItem.Price);
            Assert.Equal(2.5 * 1.99, receiptItem.TotalPrice);
            Assert.Equal(2.5, receiptItem.Quantity);
        }
    }
}