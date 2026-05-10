using SupermarketReceipt.Discounts;
using System.Collections.Generic;

namespace SupermarketReceipt;

/// <summary>
/// It does distribute deductions of price across cart items. 
/// </summary>
public class Teller(ISupermarketCatalog catalog)
{
    private readonly ISupermarketCatalog _catalog = catalog;
    private readonly Dictionary<Product, IDiscountPolicy> _discountPoliciesByProduct = [];

    /// <summary>
    /// Registers a special offer for a product.
    /// </summary>
    /// <param name="policy">The policy containing the offer type and any offer-specific values, such as percentage or bundle price.</param>
    /// <param name="product">The product the offer applies to.</param>
    public void AddSpecialOffer(IDiscountPolicy policy, Product product)
    {
        _discountPoliciesByProduct[product] = policy;
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
            var productPrice = quantity.Amount * unitPrice;
            receipt.AddProduct(product, quantity, unitPrice, productPrice);
        }

        AddDiscounts(cart, receipt);

        return receipt;
    }

    private void AddDiscounts(ShoppingCart cart, Receipt receipt)
    {
        foreach (var productQuantity in cart.GetProductQuantities())
        {
            var product = productQuantity.Key;
            if (!_discountPoliciesByProduct.TryGetValue(product, out var policy))
            {
                continue;
            }

            var unitPrice = _catalog.GetUnitPrice(product);
            var discount = policy.GetDiscount(product, productQuantity.Value, unitPrice);
            if (!discount.IsNone)
            {
                receipt.AddDiscount(discount);
            }
        }
    }
}