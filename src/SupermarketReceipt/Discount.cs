namespace SupermarketReceipt;

public class Discount(Product product, string description, double discountAmount)
{
    public string Description { get; } = description;
    public double DiscountAmount { get; } = discountAmount;
    public Product Product { get; } = product;
}