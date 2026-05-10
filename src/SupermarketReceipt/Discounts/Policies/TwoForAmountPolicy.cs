using System.Globalization;

namespace SupermarketReceipt.Discounts.Policies;

/// <summary>
/// Applies a fixed price when buying two units of a product.
/// </summary>
public class TwoForAmountPolicy : IDiscountPolicy
{
    private static readonly CultureInfo s_culture = CultureInfo.CreateSpecificCulture("en-GB");
    private readonly double _offerPrice;

    /// <summary>
    /// Creates a two-for-price policy.
    /// </summary>
    /// <param name="offerPrice">The total price charged for each pair of products.</param>
    public TwoForAmountPolicy(double offerPrice)
    {
        _offerPrice = offerPrice;
    }

    public Discount GetDiscount(Product product, Quantity quantity, double unitPrice)
    {
        var quantityAsInt = (int)quantity.Amount;
        if (quantityAsInt < 2)
        {
            return null;
        }

        var total = _offerPrice * (quantityAsInt / 2) + quantityAsInt % 2 * unitPrice;
        var discountAmount = unitPrice * quantity.Amount - total;

        return new Discount(product, "2 for " + PrintPrice(_offerPrice), -discountAmount);
    }

    private static string PrintPrice(double price)
    {
        return price.ToString("N2", s_culture);
    }
}
