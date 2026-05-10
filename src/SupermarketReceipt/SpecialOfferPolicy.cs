namespace SupermarketReceipt;

/// <summary>
/// Calculates the discount for a configured special offer.
/// </summary>
public interface ISpecialOfferPolicy
{
    /// <summary>
    /// Returns the discount for the offer, or null when the offer does not apply to the purchased quantity.
    /// </summary>
    /// <param name="offer">The configured offer.</param>
    /// <param name="quantity">The purchased quantity for the offered product.</param>
    /// <param name="unitPrice">The regular unit price of the offered product.</param>
    Discount GetDiscount(Offer offer, Quantity quantity, double unitPrice);
}
