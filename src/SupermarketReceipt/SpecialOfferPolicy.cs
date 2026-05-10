using System.Globalization;

namespace SupermarketReceipt;

/// <summary>
/// Calculates the discount for a configured special offer.
/// </summary>
public interface ISpecialOfferPolicy
{
    /// <summary>
    /// Returns the discount for the offer, or null when the offer does not apply to the purchased quantity.
    /// </summary>
    /// <param name="offer">The configured offer.</param>
    /// <param name="quantity">The purchased quantity for the offered product.</param>
    /// <param name="unitPrice">The regular unit price of the offered product.</param>
    Discount GetDiscount(Offer offer, Quantity quantity, double unitPrice);
}

/// <summary>
/// Applies a buy-three-pay-for-two discount.
/// </summary>
public class ThreeForTwoOfferPolicy : ISpecialOfferPolicy
{
    public Discount GetDiscount(Offer offer, Quantity quantity, double unitPrice)
    {
        var quantityAsInt = (int)quantity.Amount;
        if (quantityAsInt <= 2)
        {
            return null;
        }

        var numberOfOffers = quantityAsInt / 3;
        var total = numberOfOffers * 2 * unitPrice + quantityAsInt % 3 * unitPrice;
        var discountAmount = quantity.Amount * unitPrice - total;

        return new Discount(offer.Product, "3 for 2", -discountAmount);
    }
}

/// <summary>
/// Applies a percentage discount to the total product price.
/// </summary>
public class TenPercentDiscountPolicy : ISpecialOfferPolicy
{
    private readonly double _percentage;

    /// <summary>
    /// Creates a percentage discount policy.
    /// </summary>
    /// <param name="percentage">The discount percentage, for example 10 for ten percent off.</param>
    public TenPercentDiscountPolicy(double percentage)
    {
        _percentage = percentage;
    }

    public Discount GetDiscount(Offer offer, Quantity quantity, double unitPrice)
    {
        var discountAmount = quantity.Amount * unitPrice * _percentage / 100.0;

        return new Discount(offer.Product, _percentage + "% off", -discountAmount);
    }
}

/// <summary>
/// Applies a fixed price when buying two units of a product.
/// </summary>
public class TwoForAmountOfferPolicy : ISpecialOfferPolicy
{
    private static readonly CultureInfo s_culture = CultureInfo.CreateSpecificCulture("en-GB");
    private readonly double _offerPrice;

    /// <summary>
    /// Creates a two-for-price policy.
    /// </summary>
    /// <param name="offerPrice">The total price charged for each pair of products.</param>
    public TwoForAmountOfferPolicy(double offerPrice)
    {
        _offerPrice = offerPrice;
    }

    public Discount GetDiscount(Offer offer, Quantity quantity, double unitPrice)
    {
        var quantityAsInt = (int)quantity.Amount;
        if (quantityAsInt < 2)
        {
            return null;
        }

        var total = _offerPrice * (quantityAsInt / 2) + quantityAsInt % 2 * unitPrice;
        var discountAmount = unitPrice * quantity.Amount - total;

        return new Discount(offer.Product, "2 for " + PrintPrice(_offerPrice), -discountAmount);
    }

    private static string PrintPrice(double price)
    {
        return price.ToString("N2", s_culture);
    }
}

/// <summary>
/// Applies a fixed price when buying five units of a product.
/// </summary>
public class FiveForAmountOfferPolicy : ISpecialOfferPolicy
{
    private static readonly CultureInfo s_culture = CultureInfo.CreateSpecificCulture("en-GB");
    private readonly double _offerPrice;

    /// <summary>
    /// Creates a five-for-price policy.
    /// </summary>
    /// <param name="offerPrice">The total price charged for each group of five products.</param>
    public FiveForAmountOfferPolicy(double offerPrice)
    {
        _offerPrice = offerPrice;
    }

    public Discount GetDiscount(Offer offer, Quantity quantity, double unitPrice)
    {
        var quantityAsInt = (int)quantity.Amount;
        if (quantityAsInt < 5)
        {
            return null;
        }

        var numberOfOffers = quantityAsInt / 5;
        var discountTotal = unitPrice * quantity.Amount - (_offerPrice * numberOfOffers + quantityAsInt % 5 * unitPrice);

        return new Discount(offer.Product, "5 for " + PrintPrice(_offerPrice), -discountTotal);
    }

    private static string PrintPrice(double price)
    {
        return price.ToString("N2", s_culture);
    }
}
