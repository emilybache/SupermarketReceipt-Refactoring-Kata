namespace SupermarketReceipt;

/// <summary>
/// Configures a product-specific special offer by pairing the product with the policy that calculates its discount.
/// </summary>
public class Offer
{
    private readonly ISpecialOfferPolicy _policy;

    public Offer(ISpecialOfferPolicy policy, Product product)
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