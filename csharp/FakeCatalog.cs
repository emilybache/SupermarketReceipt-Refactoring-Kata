
using System.Collections;
using System.Collections.Generic;
using System.Reflection;

namespace supermarket {
	public class FakeCatalog : SupermarketCatalog {
    private IDictionary<string, Product> products = new Dictionary<string, Product>();
    private IDictionary<string, double> prices = new Dictionary<string, double>();

    public void addProduct(Product product, double price) {
        this.products.Add(product.getName(), product);
        this.prices.Add(product.getName(), price);
    }

    public double getUnitPrice(Product p) {
        return this.prices[p.getName()];
    }
}
}