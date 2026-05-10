namespace SupermarketReceipt;

public interface ISupermarketCatalog
{
    void AddProduct(Product product, double price);

    double GetUnitPrice(Product product);
}
