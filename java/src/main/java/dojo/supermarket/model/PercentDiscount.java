package dojo.supermarket.model;

import java.util.Optional;

public class PercentDiscount implements Offer {
    private final double percentage;

    public PercentDiscount(final double percentage) {
        this.percentage = percentage;
    }

    @Override
    public Optional<Discount> calculateDiscount(final double unitPrice, final Product p, final double quantity) {
        return Optional.of(new Discount(p, percentage + "% off", quantity * unitPrice * percentage / 100.0));
    }
}
