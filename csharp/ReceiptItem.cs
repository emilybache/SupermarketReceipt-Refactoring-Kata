namespace supermarket
{
    public class ReceiptItem
    {
        private Product product;
        private double price;
        private double totalPrice;
        private double quantity;

        public ReceiptItem(Product p, double quantity, double price, double totalPrice)
        {
            this.product = p;
            this.quantity = quantity;
            this.price = price;
            this.totalPrice = totalPrice;
        }

        public double getPrice()
        {
            return this.price;
        }

        public Product getProduct()
        {
            return product;
        }

        public double getQuantity()
        {
            return quantity;
        }

        public double getTotalPrice()
        {
            return totalPrice;
        }

    }

}
