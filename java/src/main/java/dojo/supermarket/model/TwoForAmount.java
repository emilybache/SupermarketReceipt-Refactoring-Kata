package dojo.supermarket.model;

import java.util.Optional;

public class TwoForAmount implements Offer {
    public static final int TWO_ITEMS = 2;
    private final double amount;

    public TwoForAmount(final double amount) {
        this.amount = amount;
    }

    @Override
    public Optional<Discount> calculateDiscount(final double unitPrice, final Product p, final double quantity) {
        int quantityAsInt = (int) quantity;
        if (quantityAsInt < TWO_ITEMS) {
            return Optional.empty();
        }

        double total = amount * quantityAsInt / TWO_ITEMS + quantityAsInt % TWO_ITEMS * unitPrice;
        double discountN = unitPrice * quantity - total;
        return Optional.of(new Discount(p, TWO_ITEMS + " for " + amount, discountN));
    }
}
