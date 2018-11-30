using ApprovalTests;
using ApprovalTests.Reporters;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace supermarket
{
    [UseReporter(typeof(DiffReporter))]
    [TestClass]
    public class SupermarketTest
    {
        private SupermarketCatalog catalog;
        private Teller teller;
        private ShoppingCart theCart;
        private Product toothbrush;
        private Product rice;
        private Product apples;
        private Product cherryTomatoes;

        [TestInitialize]
        public void setUp()
        {
            catalog = new FakeCatalog();
            teller = new Teller(catalog);
            theCart = new ShoppingCart();

            toothbrush = new Product("toothbrush", ProductUnit.Each);
            catalog.addProduct(toothbrush, 0.99);
            rice = new Product("rice", ProductUnit.Each);
            catalog.addProduct(rice, 2.99);
            apples = new Product("apples", ProductUnit.Kilo);
            catalog.addProduct(apples, 1.99);
            cherryTomatoes = new Product("cherry tomato box", ProductUnit.Each);
            catalog.addProduct(cherryTomatoes, 0.69);

        }

        [TestMethod]
        public void an_empty_shopping_cart_should_cost_nothing()
        {
            Receipt receipt = teller.checksOutArticlesFrom(theCart);
            Approvals.Verify(new ReceiptPrinter(40).printReceipt(receipt));
        }

        [TestMethod]
        public void one_normal_item()
        {
            theCart.addItem(toothbrush);
            Receipt receipt = teller.checksOutArticlesFrom(theCart);
            Approvals.Verify(new ReceiptPrinter(40).printReceipt(receipt));
        }

        [TestMethod]
        public void two_normal_items()
        {
            theCart.addItem(toothbrush);
            theCart.addItem(rice);
            Receipt receipt = teller.checksOutArticlesFrom(theCart);
            Approvals.Verify(new ReceiptPrinter(40).printReceipt(receipt));
        }

        [TestMethod]
        public void buy_two_get_one_free()
        {
            theCart.addItem(toothbrush);
            theCart.addItem(toothbrush);
            theCart.addItem(toothbrush);
            teller.addSpecialOffer(SpecialOfferType.ThreeForTwo, toothbrush, catalog.getUnitPrice(toothbrush));
            Receipt receipt = teller.checksOutArticlesFrom(theCart);
            Approvals.Verify(new ReceiptPrinter(40).printReceipt(receipt));
        }

        [TestMethod]
        public void buy_five_get_one_free()
        {
            theCart.addItem(toothbrush);
            theCart.addItem(toothbrush);
            theCart.addItem(toothbrush);
            theCart.addItem(toothbrush);
            theCart.addItem(toothbrush);
            teller.addSpecialOffer(SpecialOfferType.ThreeForTwo, toothbrush, catalog.getUnitPrice(toothbrush));
            Receipt receipt = teller.checksOutArticlesFrom(theCart);
            Approvals.Verify(new ReceiptPrinter(40).printReceipt(receipt));
        }

        [TestMethod]
        public void loose_weight_product()
        {
            theCart.addItemQuantity(apples, .5);
            Receipt receipt = teller.checksOutArticlesFrom(theCart);
            Approvals.Verify(new ReceiptPrinter(40).printReceipt(receipt));
        }

        [TestMethod]
        public void percent_discount()
        {
            theCart.addItem(rice);
            teller.addSpecialOffer(SpecialOfferType.TenPercentDiscount, rice, 10.0);
            Receipt receipt = teller.checksOutArticlesFrom(theCart);
            Approvals.Verify(new ReceiptPrinter(40).printReceipt(receipt));
        }

        [TestMethod]
        public void xForY_discount()
        {
            theCart.addItem(cherryTomatoes);
            theCart.addItem(cherryTomatoes);
            teller.addSpecialOffer(SpecialOfferType.TwoForAmount, cherryTomatoes, .99);
            Receipt receipt = teller.checksOutArticlesFrom(theCart);
            Approvals.Verify(new ReceiptPrinter(40).printReceipt(receipt));
        }

        [TestMethod]
        public void FiveForY_discount()
        {
            theCart.addItemQuantity(apples, 5);
            teller.addSpecialOffer(SpecialOfferType.FiveForAmount, apples, 6.99);
            Receipt receipt = teller.checksOutArticlesFrom(theCart);
            Approvals.Verify(new ReceiptPrinter(40).printReceipt(receipt));
        }

        [TestMethod]
        public void FiveForY_discount_withSix()
        {
            theCart.addItemQuantity(apples, 6);
            teller.addSpecialOffer(SpecialOfferType.FiveForAmount, apples, 6.99);
            Receipt receipt = teller.checksOutArticlesFrom(theCart);
            Approvals.Verify(new ReceiptPrinter(40).printReceipt(receipt));
        }

        [TestMethod]
        public void FiveForY_discount_withSixteen()
        {
            theCart.addItemQuantity(apples, 16);
            teller.addSpecialOffer(SpecialOfferType.FiveForAmount, apples, 6.99);
            Receipt receipt = teller.checksOutArticlesFrom(theCart);
            Approvals.Verify(new ReceiptPrinter(40).printReceipt(receipt));
        }

        [TestMethod]
        public void FiveForY_discount_withFour()
        {
            theCart.addItemQuantity(apples, 4);
            teller.addSpecialOffer(SpecialOfferType.FiveForAmount, apples, 6.99);
            Receipt receipt = teller.checksOutArticlesFrom(theCart);
            Approvals.Verify(new ReceiptPrinter(40).printReceipt(receipt));
        }
    }
}
