namespace SupermarketReceipt
{
    public class Discount
    {
        public Discount(Product product, string description, double discountAmount)
        {
            Product = product;
            Description = description;
            DiscountAmount = discountAmount;
        }

        public string Description { get; }
        public double DiscountAmount { get; }
        public Product Product { get; }
    }
}