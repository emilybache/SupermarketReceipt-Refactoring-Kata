namespace SupermarketReceipt.Discounts;

/// <summary>
/// Binds product and policy.
/// </summary>
public class Offer
{
    private readonly IDiscountPolicy _policy;

    public Offer(IDiscountPolicy policy, Product product)
    {
        _policy = policy;
        Product = product;
    }

    public Product Product { get; }

    public Discount GetDiscount(Quantity quantity, double unitPrice)
    {
        return _policy.GetDiscount(Product, quantity, unitPrice);
    }
}
// todo: refactor as this looks like not proper place