using System;
using System.Collections.Generic;

namespace supermarket
{
    public class ShoppingCart
    {

        private List<ProductQuantity> items = new List<ProductQuantity>();
        Dictionary<Product, double> productQuantities = new Dictionary<Product, double>();


        public List<ProductQuantity> getItems()
        {
            return new List<ProductQuantity>(items);
        }

        public void addItem(Product product)
        {
            this.addItemQuantity(product, 1.0);
        }


        public void addItemQuantity(Product product, double quantity)
        {
            items.Add(new ProductQuantity(product, quantity));
            if (productQuantities.ContainsKey(product))
            {
                productQuantities.Add(product, productQuantities[product] + quantity);
            }
            else
            {
                productQuantities.Add(product, quantity);
            }
        }

        public void handleOffers(Receipt receipt, Dictionary<Product, Offer> offers, SupermarketCatalog catalog)
        {
            foreach (Product p in productQuantities.Keys)
            {
                double quantity = productQuantities[p];
                if (offers.ContainsKey(p))
                {
                    Offer offer = offers[p];
                    double unitPrice = catalog.getUnitPrice(p);
                    int quantityAsInt = (int)quantity;
                    Discount discount = null;
                    int x = 1;
                    if (offer.offerType == SpecialOfferType.ThreeForTwo)
                    {
                        x = 3;

                    }
                    else if (offer.offerType == SpecialOfferType.TwoForAmount)
                    {
                        x = 2;
                        if (quantityAsInt >= 2)
                        {
                            double total = offer.argument * quantityAsInt / x + quantityAsInt % 2 * unitPrice;
                            double discountN = unitPrice * quantity - total;
                            discount = new Discount(p, "2 for " + offer.argument, discountN);
                        }

                    }
                    if (offer.offerType == SpecialOfferType.FiveForAmount)
                    {
                        x = 5;
                    }
                    int numberOfXs = quantityAsInt / x;
                    if (offer.offerType == SpecialOfferType.ThreeForTwo && quantityAsInt > 2)
                    {
                        double discountAmount = quantity * unitPrice - ((numberOfXs * 2 * unitPrice) + quantityAsInt % 3 * unitPrice);
                        discount = new Discount(p, "3 for 2", discountAmount);
                    }
                    if (offer.offerType == SpecialOfferType.TenPercentDiscount)
                    {
                        discount = new Discount(p, offer.argument + "% off", quantity * unitPrice * offer.argument / 100.0);
                    }
                    if (offer.offerType == SpecialOfferType.FiveForAmount && quantityAsInt >= 5)
                    {
                        double discountTotal = unitPrice * quantity - (offer.argument * numberOfXs + quantityAsInt % 5 * unitPrice);
                        discount = new Discount(p, x + " for " + offer.argument, discountTotal);
                    }
                    if (discount != null)
                        receipt.addDiscount(discount);
                }

            }
        }
    }

}
