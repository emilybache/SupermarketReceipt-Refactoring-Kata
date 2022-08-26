package dojo.supermarket.model;

public class Offer {

    SpecialOfferType offerType;
    private final Product product;
    double argument;

    public Offer(SpecialOfferType offerType, Product product, double argument) {
        this.offerType = offerType;
        this.argument = argument;
        this.product = product;
    }

    Product getProduct() {
        return product;
    }
}
