namespace SupermarketReceipt.Offers;

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

    public Discount GetDiscount(Product product, Quantity quantity, double unitPrice)
    {
        var discountAmount = quantity.Amount * unitPrice * _percentage / 100.0;

        return new Discount(product, _percentage + "% off", -discountAmount);
    }
}
