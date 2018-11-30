namespace supermarket
{

    public class Product
    {
        private  string name;
        private ProductUnit unit;

        public Product(string name, ProductUnit unit)
        {
            this.name = name;
            this.unit = unit;
        }

        public string getName()
        {
            return name;
        }


        public ProductUnit getUnit()
        {
            return unit;
        }

       
    }
}
