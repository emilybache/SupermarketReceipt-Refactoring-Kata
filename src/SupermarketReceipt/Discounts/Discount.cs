namespace SupermarketReceipt.Discounts;

public record Discount(Product Product, string Description, double DiscountAmount);