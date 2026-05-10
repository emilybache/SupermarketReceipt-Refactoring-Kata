using System.Globalization;

namespace SupermarketReceipt.Discounts.Policies;

/// <summary>
/// Applies a fixed price when buying five units of a product.
/// </summary>
public class FiveForAmountPolicy : IDiscountPolicy
{
    private static readonly CultureInfo s_culture = CultureInfo.CreateSpecificCulture("en-GB");
    private readonly double _offerPrice;

    /// <summary>
    /// Creates a five-for-price policy.
    /// </summary>
    /// <param name="offerPrice">The total price charged for each group of five products.</param>
    public FiveForAmountPolicy(double offerPrice)
    {
        _offerPrice = offerPrice;
    }

    public Discount GetDiscount(Product product, Quantity quantity, double unitPrice)
    {
        var quantityAsInt = (int)quantity.Amount;
        if (quantityAsInt < 5)
        {
            return null;
        }

        var numberOfOffers = quantityAsInt / 5;
        var discountTotal = unitPrice * quantity.Amount - (_offerPrice * numberOfOffers + quantityAsInt % 5 * unitPrice);

        return new Discount(product, "5 for " + PrintPrice(_offerPrice), -discountTotal);
    }

    private static string PrintPrice(double price)
    {
        return price.ToString("N2", s_culture);
    }
}
