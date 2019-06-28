using System.Collections.Generic;

namespace SupermarketReceipt
{
    public class Receipt
    {
        private readonly List<ReceiptItem> _items = new List<ReceiptItem>();
        private readonly List<Discount> _discounts = new List<Discount>();

        public double GetTotalPrice()
        {
            double total = 0.0;
            foreach (var item in this._items)
            {
                total += item.TotalPrice;
            }
            foreach (var discount in this._discounts)
            {
                total -= discount.DiscountAmount;
            }
            return total;
        }

        public void AddProduct(Product p, double quantity, double price, double totalPrice)
        {
            this._items.Add(new ReceiptItem(p, quantity, price, totalPrice));
        }

        public List<ReceiptItem> GetItems()
        {
            return new List<ReceiptItem>(this._items);
        }

        public void AddDiscount(Discount discount)
        {
            this._discounts.Add(discount);
        }

        public List<Discount> GetDiscounts()
        {
            return _discounts;
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
