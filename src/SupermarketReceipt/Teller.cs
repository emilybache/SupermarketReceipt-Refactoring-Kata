using System.Collections.Generic;

namespace SupermarketReceipt;

public class Teller
{
    private readonly ISupermarketCatalog _catalog;
    private readonly IOfferDiscountCalculator _offerDiscountCalculator;
    private readonly Dictionary<Product, Offer> _offers = new Dictionary<Product, Offer>();

    public Teller(ISupermarketCatalog catalog) : this(catalog, SpecialOfferDiscountCalculator.CreateDefault())
    {
    }

    public Teller(ISupermarketCatalog catalog, IOfferDiscountCalculator offerDiscountCalculator)
    {
        _catalog = catalog;
        _offerDiscountCalculator = offerDiscountCalculator;
    }

    public void AddSpecialOffer(SpecialOfferType offerType, Product product, double argument)
    {
        _offers[product] = new Offer(offerType, product, argument);
    }

    public Receipt ChecksOutArticlesFrom(ShoppingCart cart)
    {
        var receipt = new Receipt();
        var productQuantities = cart.GetItems();
        foreach (var productQuantity in productQuantities)
        {
            var product = productQuantity.Product;
            var quantity = productQuantity.Quantity;
            var unitPrice = _catalog.GetUnitPrice(product);
            var price = quantity.Amount * unitPrice;
            receipt.AddProduct(product, quantity, unitPrice, price);
        }

        AddDiscounts(cart, receipt);

        return receipt;
    }

    private void AddDiscounts(ShoppingCart cart, Receipt receipt)
    {
        foreach (var productQuantity in cart.GetProductQuantities())
        {
            var product = productQuantity.Key;
            if (!_offers.TryGetValue(product, out var offer))
            {
                continue;
            }

            var unitPrice = _catalog.GetUnitPrice(product);
            var discount = _offerDiscountCalculator.GetDiscount(offer, productQuantity.Value, unitPrice);
            if (discount != null)
            {
                receipt.AddDiscount(discount);
            }
        }
    }
}