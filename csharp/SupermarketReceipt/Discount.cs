namespace SupermarketReceipt
{

    public class Discount
    {
        public string Description { get; }
        public double DiscountAmount { get; }
        public Product Product { get; }

        public Discount(Product product, string description, double discountAmount)
        {
            this.Product = product;
            this.Description = description;
            this.DiscountAmount = discountAmount;
        }

    }
}
