namespace SupermarketReceipt;

/// <summary>
/// Configures a product-specific special offer by pairing the product with the policy that calculates its discount.
/// </summary>
public class Offer
{
    public Offer(ISpecialOfferPolicy policy, Product product)
    {
        Policy = policy;
        Product = product;
    }

    public ISpecialOfferPolicy Policy { get; }
    public Product Product { get; }
}