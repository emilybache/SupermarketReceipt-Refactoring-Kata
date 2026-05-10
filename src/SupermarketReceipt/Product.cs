using System;
using System.Globalization;

namespace SupermarketReceipt;

public record Product(string Name, ProductUnit Unit);

public readonly record struct Quantity(double Amount, ProductUnit Unit)
{
    public string Format(CultureInfo culture)
    {
        return Unit.Format(Amount, culture);
    }
}

public class ProductQuantity
{
    public ProductQuantity(Product product, Quantity quantity)
    {
        Product = product;
        Quantity = quantity;
    }

    public Product Product { get; }
    public Quantity Quantity { get; }
}

public sealed class ProductUnit
{
    public static readonly ProductUnit Kilo = new ProductUnit("kilo", (amount, culture) => amount.ToString("N3", culture));
    public static readonly ProductUnit Each = new ProductUnit("each", (amount, culture) => ((int)amount).ToString(culture));

    private readonly Func<double, CultureInfo, string> _quantityFormatter;

    public ProductUnit(string name, Func<double, CultureInfo, string> quantityFormatter)
    {
        Name = name;
        _quantityFormatter = quantityFormatter;
    }

    public string Name { get; }

    public string Format(double amount, CultureInfo culture)
    {
        return _quantityFormatter(amount, culture);
    }
}