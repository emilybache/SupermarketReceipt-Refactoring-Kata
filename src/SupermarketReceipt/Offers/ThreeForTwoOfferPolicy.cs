namespace SupermarketReceipt.Offers;

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
