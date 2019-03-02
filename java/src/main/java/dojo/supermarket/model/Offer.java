package dojo.supermarket.model;

import java.util.Optional;

public interface  Offer {

    static Offer createOffer(SpecialOfferType offerType, double argument) {
        switch (offerType) {
            case ThreeForTwo: {
                return new ThreeForTwo();
            }
            case TwoForAmount: {
                return new TwoForAmount(argument);
            }
            case FiveForAmount: {
                return new FiveForAmount(argument);
            }
            case PercentDiscount: {
                return new PercentDiscount(argument);
            }
            default:
                throw new IllegalArgumentException("Invalid offerType: " + offerType.name());
        }
    }

    Optional<Discount> calculateDiscount(final double unitPrice, final Product p, final double quantity);
}
