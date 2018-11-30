using System.Collections.Generic;

namespace supermarket
{

    public class Teller
    {

        private SupermarketCatalog catalog;
        private Dictionary<Product, Offer> offers = new Dictionary<Product, Offer>();

        public Teller(SupermarketCatalog catalog)
        {
            this.catalog = catalog;
        }

        public void addSpecialOffer(SpecialOfferType offerType, Product product, double argument)
        {
            this.offers.Add(product, new Offer(offerType, product, argument));
        }

        public Receipt checksOutArticlesFrom(ShoppingCart theCart)
        {
            Receipt receipt = new Receipt();
            List<ProductQuantity> productQuantities = theCart.getItems();
            foreach (ProductQuantity pq in
            productQuantities) {
                Product p = pq.getProduct();
                double quantity = pq.getQuantity();
                double unitPrice = this.catalog.getUnitPrice(p);
                double price = quantity * unitPrice;
                receipt.addProduct(p, quantity, unitPrice, price);
            }
            theCart.handleOffers(receipt, this.offers, this.catalog);

            return receipt;
        }

    }
}
