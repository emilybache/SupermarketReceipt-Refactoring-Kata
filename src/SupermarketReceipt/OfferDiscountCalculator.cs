using System.Collections.Generic;
using System.Linq;

namespace SupermarketReceipt;

public interface IOfferDiscountCalculator
{
    Discount GetDiscount(Offer offer, Quantity quantity, double unitPrice);
}

public class SpecialOfferDiscountCalculator : IOfferDiscountCalculator
{
    private readonly IEnumerable<ISpecialOfferPolicy> _policies;

    public SpecialOfferDiscountCalculator(IEnumerable<ISpecialOfferPolicy> policies)
    {
        _policies = policies;
    }

    public static SpecialOfferDiscountCalculator CreateDefault()
    {
        return new SpecialOfferDiscountCalculator(
        [
            new ThreeForTwoOfferPolicy(),
            new TenPercentDiscountPolicy(),
            new TwoForAmountOfferPolicy(),
            new FiveForAmountOfferPolicy()
        ]);
    }

    public Discount GetDiscount(Offer offer, Quantity quantity, double unitPrice)
    {
        var policy = _policies.FirstOrDefault(policy => policy.CanApply(offer.OfferType));

        return policy?.GetDiscount(offer, quantity, unitPrice);
    }
}
