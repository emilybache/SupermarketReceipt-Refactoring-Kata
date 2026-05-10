namespace SupermarketReceipt;

/// <summary>
/// Ensures unique products in inventory havin prices defined.
/// </summary>
public interface ISupermarketCatalog
{
    void AddProduct(Product product, double price);

    double GetUnitPrice(Product product);
}
