package dojo.supermarket.model;

public abstract class Offer {
    SpecialOfferType offerType;
    double argument;

    Offer(SpecialOfferType offerType, double argument) {
        this.offerType = offerType;
        this.argument = argument;
    }

    public static Offer createOffer(SpecialOfferType offerType, Product product, double argument) {
        switch (offerType) {
            case ThreeForTwo: {
                return new ThreeForTwo(product, argument);
            }
            case TwoForAmount: {
                return new TwoForAmount(product, argument);
            }
            case FiveForAmount: {
                return new FiveForAmount(product, argument);
            }
            case TenPercentDiscount: {
                return new TenPercentDiscount(product, argument);
            }
            default:
                throw new IllegalArgumentException("Invalid offerType: " + offerType.name());
        }
    }

    protected abstract void doHandleOffers(final Receipt receipt, final SupermarketCatalog catalog, final Product p, final ShoppingCart shoppingCart);
}
