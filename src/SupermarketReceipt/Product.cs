using System.Collections.Generic;

namespace SupermarketReceipt
{
    public record Product(string Name, ProductUnit Unit);

    public class ProductQuantity
    {
        public ProductQuantity(Product product, double weight)
        {
            Product = product;
            Quantity = weight;
        }

        public Product Product { get; }
        public double Quantity { get; }
    }

    public enum ProductUnit
    {
        Kilo,
        Each
    }
}