namespace SupermarketReceipt.Discounts;

public record Discount(Product Product, string Description, double DiscountAmount)
{
    /// <summary>
    /// Represents the absence of a discount — returned by policies when the offer does not apply.
    /// </summary>
    public static Discount None { get; } = new NoDiscount();

    /// <summary>
    /// True when this instance represents no discount (the offer did not apply).
    /// </summary>
    public virtual bool IsNone => false;

    private sealed record NoDiscount() : Discount(null!, string.Empty, 0)
    {
        public override bool IsNone => true;
    }
}