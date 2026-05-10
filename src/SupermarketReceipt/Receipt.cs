using System.Collections.Generic;
using SupermarketReceipt.Discounts;

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

public record ReceiptItem(Product Product, Quantity Quantity, double Price, double TotalPrice);
