namespace supermarket
{
    public class Offer
    {
        public SpecialOfferType offerType { get; }
        private Product product;
        public double argument { get; }

        public Offer(SpecialOfferType offerType, Product product, double argument)
        {
            this.offerType = offerType;
            this.argument = argument;
            this.product = product;
        }

    }


}

