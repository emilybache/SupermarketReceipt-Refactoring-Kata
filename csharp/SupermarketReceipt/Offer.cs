using System;

namespace SupermarketReceipt
{
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
            Product = product ?? throw new ArgumentNullException(nameof(product), "Product cannot be null.");
            Argument = argument < 0
                ? throw new ArgumentException("Argument must be non-negative.", nameof(argument))
                : argument;
        }

        public SpecialOfferType OfferType { get; }
        public Product Product { get; }
        public double Argument { get; }
    }
}