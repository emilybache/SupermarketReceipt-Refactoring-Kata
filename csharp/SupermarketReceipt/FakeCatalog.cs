using System.Collections.Generic;

namespace SupermarketReceipt {
	public class FakeCatalog : SupermarketCatalog {
    private readonly IDictionary<string, Product> _products = new Dictionary<string, Product>();
    private readonly IDictionary<string, double> _prices = new Dictionary<string, double>();

    public void AddProduct(Product product, double price) {
        this._products.Add(product.Name, product);
        this._prices.Add(product.Name, price);
    }

    public double GetUnitPrice(Product p) {
        return this._prices[p.Name];
    }
}
}