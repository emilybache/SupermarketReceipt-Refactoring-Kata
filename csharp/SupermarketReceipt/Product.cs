using System.Collections.Generic;

namespace SupermarketReceipt
{
    public class Product
    {
        public  string Name { get; }
        public ProductUnit Unit { get; }

        public Product(string name, ProductUnit unit)
        {
            this.Name = name;
            this.Unit = unit;
        }

        public override bool Equals(object obj)
        {
            var product = obj as Product;
            return product != null &&
                   Name == product.Name &&
                   Unit == product.Unit;
        }

        public override int GetHashCode()
        {
            var hashCode = -1996304355;
            hashCode = hashCode * -1521134295 + EqualityComparer<string>.Default.GetHashCode(Name);
            hashCode = hashCode * -1521134295 + Unit.GetHashCode();
            return hashCode;
        }
    }

    public class ProductQuantity
    {
        public Product Product { get; }
        public double Quantity { get; }

        public ProductQuantity(Product product, double weight)
        {
            this.Product = product;
            this.Quantity = weight;
        }

    }

    public enum ProductUnit
    {
        Kilo, Each
    }
}
