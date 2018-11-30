using System.Collections.Generic;

namespace supermarket
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
            this._offers.Add(product, new Offer(offerType, product, argument));
        }

        public Receipt ChecksOutArticlesFrom(ShoppingCart theCart)
        {
            Receipt receipt = new Receipt();
            List<ProductQuantity> productQuantities = theCart.GetItems();
            foreach (ProductQuantity pq in
            productQuantities) {
                Product p = pq.Product;
                double quantity = pq.Quantity;
                double unitPrice = this._catalog.GetUnitPrice(p);
                double price = quantity * unitPrice;
                receipt.addProduct(p, quantity, unitPrice, price);
            }
            theCart.HandleOffers(receipt, this._offers, this._catalog);

            return receipt;
        }

    }
}
