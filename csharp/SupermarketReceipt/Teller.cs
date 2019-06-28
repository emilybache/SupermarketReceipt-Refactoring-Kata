using System.Collections.Generic;

namespace SupermarketReceipt
{

    public class Teller
    {

        private readonly SupermarketCatalog _catalog;
        private readonly Dictionary<Product, Offer> _offers = new Dictionary<Product, Offer>();

        public Teller(SupermarketCatalog catalog)
        {
            this._catalog = catalog;
        }

        public void AddSpecialOffer(SpecialOfferType offerType, Product product, double argument)
        {
            this._offers[product] = new Offer(offerType, product, argument);
        }

        public Receipt ChecksOutArticlesFrom(ShoppingCart theCart)
        {
            var receipt = new Receipt();
            var productQuantities = theCart.GetItems();
            foreach (ProductQuantity pq in productQuantities) {
                var p = pq.Product;
                var quantity = pq.Quantity;
                var unitPrice = this._catalog.GetUnitPrice(p);
                var price = quantity * unitPrice;
                receipt.AddProduct(p, quantity, unitPrice, price);
            }
            theCart.HandleOffers(receipt, this._offers, this._catalog);

            return receipt;
        }

    }
}
