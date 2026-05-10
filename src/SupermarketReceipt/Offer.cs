namespace SupermarketReceipt;

public class Offer
{
    public Offer(ISpecialOfferPolicy policy, Product product, double argument)
    {
        Policy = policy;
        Argument = argument;
        Product = product;
    }

    public ISpecialOfferPolicy Policy { get; }
    public double Argument { get; }
    public Product Product { get; }
}