using System.Globalization;

namespace SupermarketReceipt;

public interface ISpecialOfferPolicy
{
    bool CanApply(SpecialOfferType offerType);

    Discount GetDiscount(Offer offer, Quantity quantity, double unitPrice);
}

public class ThreeForTwoOfferPolicy : ISpecialOfferPolicy
{
    public bool CanApply(SpecialOfferType offerType)
    {
        return offerType == SpecialOfferType.ThreeForTwo;
    }

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

public class TenPercentDiscountPolicy : ISpecialOfferPolicy
{
    public bool CanApply(SpecialOfferType offerType)
    {
        return offerType == SpecialOfferType.TenPercentDiscount;
    }

    public Discount GetDiscount(Offer offer, Quantity quantity, double unitPrice)
    {
        var discountAmount = quantity.Amount * unitPrice * offer.Argument / 100.0;

        return new Discount(offer.Product, offer.Argument + "% off", -discountAmount);
    }
}

public class TwoForAmountOfferPolicy : ISpecialOfferPolicy
{
    private static readonly CultureInfo s_culture = CultureInfo.CreateSpecificCulture("en-GB");

    public bool CanApply(SpecialOfferType offerType)
    {
        return offerType == SpecialOfferType.TwoForAmount;
    }

    public Discount GetDiscount(Offer offer, Quantity quantity, double unitPrice)
    {
        var quantityAsInt = (int)quantity.Amount;
        if (quantityAsInt < 2)
        {
            return null;
        }

        var total = offer.Argument * (quantityAsInt / 2) + quantityAsInt % 2 * unitPrice;
        var discountAmount = unitPrice * quantity.Amount - total;

        return new Discount(offer.Product, "2 for " + PrintPrice(offer.Argument), -discountAmount);
    }

    private static string PrintPrice(double price)
    {
        return price.ToString("N2", s_culture);
    }
}

public class FiveForAmountOfferPolicy : ISpecialOfferPolicy
{
    private static readonly CultureInfo s_culture = CultureInfo.CreateSpecificCulture("en-GB");

    public bool CanApply(SpecialOfferType offerType)
    {
        return offerType == SpecialOfferType.FiveForAmount;
    }

    public Discount GetDiscount(Offer offer, Quantity quantity, double unitPrice)
    {
        var quantityAsInt = (int)quantity.Amount;
        if (quantityAsInt < 5)
        {
            return null;
        }

        var numberOfOffers = quantityAsInt / 5;
        var discountTotal = unitPrice * quantity.Amount - (offer.Argument * numberOfOffers + quantityAsInt % 5 * unitPrice);

        return new Discount(offer.Product, "5 for " + PrintPrice(offer.Argument), -discountTotal);
    }

    private static string PrintPrice(double price)
    {
        return price.ToString("N2", s_culture);
    }
}
