using System;
using System.Collections.Generic;

namespace SupermarketReceipt
{
    public class FakeCatalog : SupermarketCatalog
    {
        private readonly IDictionary<string, (Product product, double price)> _catalog =
            new Dictionary<string, (Product product, double price)>();

        public void AddProduct(Product product, double price)
        {
            if (product == null) throw new ArgumentNullException(nameof(product));
            if (price < 0) throw new ArgumentException("Price cannot be negative.", nameof(price));

            _catalog[product.Name] = (product, price);
        }

        public double GetUnitPrice(Product product)
        {
            if (product == null) throw new ArgumentNullException(nameof(product));
            if (!_catalog.TryGetValue(product.Name, out var entry))
                throw new ArgumentException($"Product {product.Name} not found in the catalog.");

            return entry.price;
        }
    }
}