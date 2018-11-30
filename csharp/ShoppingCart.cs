using System;
using System.Collections.Generic;

namespace supermarket
{
    public class ShoppingCart
    {

        private List<ProductQuantity> items = new List<ProductQuantity>();
        Dictionary<Product, double> productQuantities = new Dictionary<Product, double>();


        public List<ProductQuantity> GetItems()
        {
            return new List<ProductQuantity>(items);
        }

        public void AddItem(Product product)
        {
            this.AddItemQuantity(product, 1.0);
        }


        public void AddItemQuantity(Product product, double quantity)
        {
            items.Add(new ProductQuantity(product, quantity));
            if (productQuantities.ContainsKey(product))
            {
                var newAmount = productQuantities[product] + quantity;
                productQuantities[product] = newAmount;
            }
            else
            {
                productQuantities.Add(product, quantity);
            }
        }

        public void HandleOffers(Receipt receipt, Dictionary<Product, Offer> offers, SupermarketCatalog catalog)
        {
            foreach (Product p in productQuantities.Keys)
            {
                double quantity = productQuantities[p];
                if (offers.ContainsKey(p))
                {
                    Offer offer = offers[p];
                    double unitPrice = catalog.GetUnitPrice(p);
                    int quantityAsInt = (int)quantity;
                    Discount discount = null;
                    int x = 1;
                    if (offer.OfferType == SpecialOfferType.ThreeForTwo)
                    {
                        x = 3;

                    }
                    else if (offer.OfferType == SpecialOfferType.TwoForAmount)
                    {
                        x = 2;
                        if (quantityAsInt >= 2)
                        {
                            double total = offer.Argument * quantityAsInt / x + quantityAsInt % 2 * unitPrice;
                            double discountN = unitPrice * quantity - total;
                            discount = new Discount(p, "2 for " + offer.Argument, discountN);
                        }

                    }
                    if (offer.OfferType == SpecialOfferType.FiveForAmount)
                    {
                        x = 5;
                    }
                    int numberOfXs = quantityAsInt / x;
                    if (offer.OfferType == SpecialOfferType.ThreeForTwo && quantityAsInt > 2)
                    {
                        double discountAmount = quantity * unitPrice - ((numberOfXs * 2 * unitPrice) + quantityAsInt % 3 * unitPrice);
                        discount = new Discount(p, "3 for 2", discountAmount);
                    }
                    if (offer.OfferType == SpecialOfferType.TenPercentDiscount)
                    {
                        discount = new Discount(p, offer.Argument + "% off", quantity * unitPrice * offer.Argument / 100.0);
                    }
                    if (offer.OfferType == SpecialOfferType.FiveForAmount && quantityAsInt >= 5)
                    {
                        double discountTotal = unitPrice * quantity - (offer.Argument * numberOfXs + quantityAsInt % 5 * unitPrice);
                        discount = new Discount(p, x + " for " + offer.Argument, discountTotal);
                    }
                    if (discount != null)
                        receipt.addDiscount(discount);
                }

            }
        }
    }

}
