using Xunit;

namespace SupermarketReceipt
{
    public class SupermarketTest
    {
        
       [Fact]
       public void TestSomething()
       {
           SupermarketCatalog catalog = new FakeCatalog();
           var toothbrush = new Product("toothbrush", ProductUnit.Each);
           catalog.AddProduct(toothbrush, 0.99);
           var apples = new Product("apples", ProductUnit.Kilo);
           catalog.AddProduct(apples, 1.99);

           var cart = new ShoppingCart();
           cart.AddItemQuantity(apples, 2.5);

           var teller = new Teller(catalog);
           teller.AddSpecialOffer(SpecialOfferType.TenPercentDiscount, toothbrush, 10.0);

           var receipt = teller.ChecksOutArticlesFrom(cart);

           Assert.Equal(4.975, receipt.GetTotalPrice());
       }
    }
}
