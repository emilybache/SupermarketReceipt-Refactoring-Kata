using System.Collections.Generic;

namespace SupermarketReceipt
{
    public class FakeCatalog : ISupermarketCatalog
    {
        private readonly IDictionary<string, double> _prices = new Dictionary<string, double>();
        private readonly IDictionary<string, Product> _products = new Dictionary<string, Product>();

        public void AddProduct(Product product, double price)
        {
            _products.Add(product.Name, product);
            _prices.Add(product.Name, price);
        }

        public double GetUnitPrice(Product product)
        {
            return _prices[product.Name];
        }
    }
}