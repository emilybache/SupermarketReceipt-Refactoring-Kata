package dojo.supermarket.model;

import java.util.Optional;

public class ThreeForTwo implements Offer {

    @Override
    public Optional<Discount> calculateDiscount(final double unitPrice, final Product p, final double quantity) {
        int quantityAsInt = (int) quantity;
        if (quantityAsInt <= 2) {
            return Optional.empty();
        }

        int THREE_ITEMS = 3;
        int numberOfXs = quantityAsInt / THREE_ITEMS;
        double discountAmount = quantity * unitPrice - ((numberOfXs * 2 * unitPrice) + quantityAsInt % 3 * unitPrice);
        return Optional.of(new Discount(p, "3 for 2", discountAmount));
    }
}
