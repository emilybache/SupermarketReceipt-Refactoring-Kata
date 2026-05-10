namespace SupermarketReceipt.Discounts.Policies;

/// <summary>
/// Applies a buy-three-pay-for-two discount.
/// </summary>
public class ThreeForTwoPolicy : IDiscountPolicy
{
    public Discount GetDiscount(Product product, Quantity quantity, double unitPrice)
    {
        var quantityAsInt = (int)quantity.Amount;
        if (quantityAsInt <= 2)
        {
            return Discount.None;
        }

        var numberOfOffers = quantityAsInt / 3;
        var total = numberOfOffers * 2 * unitPrice + quantityAsInt % 3 * unitPrice;
        var discountAmount = quantity.Amount * unitPrice - total;

        return new Discount(product, "3 for 2", -discountAmount);
    }
}
