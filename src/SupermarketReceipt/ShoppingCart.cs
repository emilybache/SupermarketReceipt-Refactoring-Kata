using System.Collections.Generic;

namespace SupermarketReceipt;

public class ShoppingCart
{
    private readonly List<ProductQuantity> _items = new List<ProductQuantity>();
    private readonly Dictionary<Product, Quantity> _productQuantities = new Dictionary<Product, Quantity>();


    public List<ProductQuantity> GetItems()
    {
        return new List<ProductQuantity>(_items);
    }

    public Dictionary<Product, Quantity> GetProductQuantities()
    {
        return new Dictionary<Product, Quantity>(_productQuantities);
    }

    public void AddItem(Product product)
    {
        AddItemQuantity(product, 1.0);
    }


    public void AddItemQuantity(Product product, double quantity)
    {
        var productQuantity = new Quantity(quantity, product.Unit);
        _items.Add(new ProductQuantity(product, productQuantity));
        if (_productQuantities.ContainsKey(product))
        {
            var currentQuantity = _productQuantities[product];
            var newQuantity = currentQuantity with { Amount = currentQuantity.Amount + productQuantity.Amount };
            _productQuantities[product] = newQuantity;
        }
        else
        {
            _productQuantities.Add(product, productQuantity);
        }
    }

}