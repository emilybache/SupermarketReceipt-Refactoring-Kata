namespace supermarket
{

    public class Discount
    {
        private string description;
        private double discountAmount;
        private Product product;

        public Discount(Product product, string description, double discountAmount)
        {
            this.product = product;
            this.description = description;
            this.discountAmount = discountAmount;
        }

        public string getDescription()
        {
            return description;
        }

        public double getDiscountAmount()
        {
            return discountAmount;
        }

        public Product getProduct()
        {
            return product;
        }

    }
}
