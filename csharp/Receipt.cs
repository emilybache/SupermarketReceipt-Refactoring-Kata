using System.Collections.Generic;

namespace supermarket
{
    public class Receipt
    {
        private List<ReceiptItem> items = new List<ReceiptItem>();
        private List<Discount> discounts = new List<Discount>();

        public double getTotalPrice()
        {
            double total = 0.0;
            foreach (ReceiptItem item in this.items)
            {
                total += item.TotalPrice;
            }
            foreach (Discount discount in this.discounts)
            {
                total -= discount.DiscountAmount;
            }
            return total;
        }

        public void addProduct(Product p, double quantity, double price, double totalPrice)
        {
            this.items.Add(new ReceiptItem(p, quantity, price, totalPrice));
        }

        public List<ReceiptItem> getItems()
        {
            return new List<ReceiptItem>(this.items);
        }

        public void addDiscount(Discount discount)
        {
            this.discounts.Add(discount);
        }

        public List<Discount> getDiscounts()
        {
            return discounts;
        }
    }

    public class ReceiptItem
    {
        public Product Product { get; }
        public double Price { get; }
        public double TotalPrice { get; }
        public double Quantity { get; }

        public ReceiptItem(Product p, double quantity, double price, double totalPrice)
        {
            this.Product = p;
            this.Quantity = quantity;
            this.Price = price;
            this.TotalPrice = totalPrice;
        }
    }
}
