package dojo.supermarket.model;

import java.util.Optional;

public class FiveForAmount implements Offer {
    public static final int FIVE_ITEMS = 5;
    private double amount;

    public FiveForAmount(final double amount) {
        this.amount = amount;
    }

    @Override
    public Optional<Discount> calculateDiscount(final double unitPrice, final Product p, final double quantity) {
        int quantityAsInt = (int) quantity;
        if (quantityAsInt < FIVE_ITEMS) {
            return Optional.empty();
        }

        int numberOfXs = quantityAsInt / FIVE_ITEMS;
        double discountTotal = unitPrice * quantity - (amount * numberOfXs + quantityAsInt % 5 * unitPrice);
        return Optional.of(new Discount(p, FIVE_ITEMS + " for " + amount, discountTotal));
    }
}
