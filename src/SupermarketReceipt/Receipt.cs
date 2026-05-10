using System.Collections.Generic;

namespace SupermarketReceipt;

public class Receipt
{
    private readonly List<Discount> _discounts = [];
    private readonly List<ReceiptItem> _items = [];

    public double GetTotalPrice()
    {
        var total = 0.0;
        foreach (var item in _items) total += item.TotalPrice;
        foreach (var discount in _discounts) total += discount.DiscountAmount;
        return total;
    }

    public void AddProduct(Product product, Quantity quantity, double price, double totalPrice)
    {
        _items.Add(new ReceiptItem(product, quantity, price, totalPrice));
    }

    public List<ReceiptItem> GetItems()
    {
        return new List<ReceiptItem>(_items);
    }

    public void AddDiscount(Discount discount)
    {
        _discounts.Add(discount);
    }

    public List<Discount> GetDiscounts()
    {
        return _discounts;
    }
}

public class ReceiptItem
{
    public ReceiptItem(Product product, Quantity quantity, double price, double totalPrice)
    {
        Product = product;
        Quantity = quantity;
        Price = price;
        TotalPrice = totalPrice;
    }

    public Product Product { get; }
    public double Price { get; }
    public double TotalPrice { get; }
    public Quantity Quantity { get; }
}