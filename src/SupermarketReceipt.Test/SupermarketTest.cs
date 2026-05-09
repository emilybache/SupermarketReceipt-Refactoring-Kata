using System.Threading.Tasks;
using VerifyXunit;
using Xunit;

namespace SupermarketReceipt.Test
{
    [UsesVerify]
    public class SupermarketTest
    {
        private SupermarketCatalog _catalog;
        private Teller _teller;
        private ShoppingCart _theCart;
        private Product _toothbrush;
        private Product _rice;
        private Product _apples;
        private Product _cherryTomatoes;

        public SupermarketTest()
        {
            _catalog = new FakeCatalog();
            _teller = new Teller(_catalog);
            _theCart = new ShoppingCart();

            _toothbrush = new Product("toothbrush", ProductUnit.Each);
            _catalog.AddProduct(_toothbrush, 0.99);
            _rice = new Product("rice", ProductUnit.Each);
            _catalog.AddProduct(_rice, 2.99);
            _apples = new Product("apples", ProductUnit.Kilo);
            _catalog.AddProduct(_apples, 1.99);
            _cherryTomatoes = new Product("cherry tomato box", ProductUnit.Each);
            _catalog.AddProduct(_cherryTomatoes, 0.69);

        }

        [Fact]
        public Task AnEmptyShoppingCartShouldCostNothing()
        {
            Receipt receipt = _teller.ChecksOutArticlesFrom(_theCart);
            return Verifier.Verify(new ReceiptPrinter(40).PrintReceipt(receipt));
        }

        [Fact]
        public Task OneNormalItem()
        {
            _theCart.AddItem(_toothbrush);
            Receipt receipt = _teller.ChecksOutArticlesFrom(_theCart);
            return Verifier.Verify(new ReceiptPrinter(40).PrintReceipt(receipt));
        }

        [Fact]
        public Task TwoNormalItems()
        {
            _theCart.AddItem(_toothbrush);
            _theCart.AddItem(_rice);
            Receipt receipt = _teller.ChecksOutArticlesFrom(_theCart);
            return Verifier.Verify(new ReceiptPrinter(40).PrintReceipt(receipt));
        }

        [Fact]
        public Task BuyTwoGetOneFree()
        {
            _theCart.AddItem(_toothbrush);
            _theCart.AddItem(_toothbrush);
            _theCart.AddItem(_toothbrush);
            _teller.AddSpecialOffer(SpecialOfferType.ThreeForTwo, _toothbrush, _catalog.GetUnitPrice(_toothbrush));
            Receipt receipt = _teller.ChecksOutArticlesFrom(_theCart);
            return Verifier.Verify(new ReceiptPrinter(40).PrintReceipt(receipt));
        }

        [Fact]
        public Task BuyFiveGetOneFree()
        {
            _theCart.AddItem(_toothbrush);
            _theCart.AddItem(_toothbrush);
            _theCart.AddItem(_toothbrush);
            _theCart.AddItem(_toothbrush);
            _theCart.AddItem(_toothbrush);
            _teller.AddSpecialOffer(SpecialOfferType.ThreeForTwo, _toothbrush, _catalog.GetUnitPrice(_toothbrush));
            Receipt receipt = _teller.ChecksOutArticlesFrom(_theCart);
            return Verifier.Verify(new ReceiptPrinter(40).PrintReceipt(receipt));
        }

        [Fact]
        public Task LooseWeightProduct()
        {
            _theCart.AddItemQuantity(_apples, .5);
            Receipt receipt = _teller.ChecksOutArticlesFrom(_theCart);
            return Verifier.Verify(new ReceiptPrinter(40).PrintReceipt(receipt));
        }

        [Fact]
        public Task PercentDiscount()
        {
            _theCart.AddItem(_rice);
            _teller.AddSpecialOffer(SpecialOfferType.TenPercentDiscount, _rice, 10.0);
            Receipt receipt = _teller.ChecksOutArticlesFrom(_theCart);
            return Verifier.Verify(new ReceiptPrinter(40).PrintReceipt(receipt));
        }

        [Fact]
        public Task XForYDiscount()
        {
            _theCart.AddItem(_cherryTomatoes);
            _theCart.AddItem(_cherryTomatoes);
            _teller.AddSpecialOffer(SpecialOfferType.TwoForAmount, _cherryTomatoes, .99);
            Receipt receipt = _teller.ChecksOutArticlesFrom(_theCart);
            return Verifier.Verify(new ReceiptPrinter(40).PrintReceipt(receipt));
        }

        [Fact]
        public Task FiveForYDiscount()
        {
            _theCart.AddItemQuantity(_apples, 5);
            _teller.AddSpecialOffer(SpecialOfferType.FiveForAmount, _apples, 6.99);
            Receipt receipt = _teller.ChecksOutArticlesFrom(_theCart);
            return Verifier.Verify(new ReceiptPrinter(40).PrintReceipt(receipt));
        }

        [Fact]
        public Task FiveForYDiscountWithSix()
        {
            _theCart.AddItemQuantity(_apples, 6);
            _teller.AddSpecialOffer(SpecialOfferType.FiveForAmount, _apples, 6.99);
            Receipt receipt = _teller.ChecksOutArticlesFrom(_theCart);
            return Verifier.Verify(new ReceiptPrinter(40).PrintReceipt(receipt));
        }

        [Fact]
        public Task FiveForYDiscountWithSixteen()
        {
            _theCart.AddItemQuantity(_apples, 16);
            _teller.AddSpecialOffer(SpecialOfferType.FiveForAmount, _apples, 6.99);
            Receipt receipt = _teller.ChecksOutArticlesFrom(_theCart);
            return Verifier.Verify(new ReceiptPrinter(40).PrintReceipt(receipt));
        }

        [Fact]
        public Task FiveForYDiscountWithFour()
        {
            _theCart.AddItemQuantity(_apples, 4);
            _teller.AddSpecialOffer(SpecialOfferType.FiveForAmount, _apples, 6.99);
            Receipt receipt = _teller.ChecksOutArticlesFrom(_theCart);
            return Verifier.Verify(new ReceiptPrinter(40).PrintReceipt(receipt));
        }
    }
}