namespace SupermarketReceipt;

public enum SpecialOfferType
{
    ThreeForTwo,
    TenPercentDiscount,
    TwoForAmount,
    FiveForAmount
}

public class Offer
{
    public Offer(SpecialOfferType offerType, Product product, double argument)
    {
        OfferType = offerType;
        Argument = argument;
        Product = product;
    }

    public SpecialOfferType OfferType { get; }
    public double Argument { get; }
    public Product Product { get; }
}