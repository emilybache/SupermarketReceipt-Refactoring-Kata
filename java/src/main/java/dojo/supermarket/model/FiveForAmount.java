package dojo.supermarket.model;

public class FiveForAmount extends Offer {
    public FiveForAmount(final Product product, final double argument) {
        super(SpecialOfferType.FiveForAmount, argument);
    }

    @Override
    protected void doHandleOffers(final Receipt receipt, final SupermarketCatalog catalog, final Product p, final ShoppingCart shoppingCart) {
        double quantity = shoppingCart.productQuantities.get(p);
        int quantityAsInt = (int) quantity;
        int x = 5;
        int numberOfXs = quantityAsInt / x;
        if (quantityAsInt >= 5) {
            double unitPrice = catalog.getUnitPrice(p);
            double discountTotal = unitPrice * quantity - (argument * numberOfXs + quantityAsInt % 5 * unitPrice);
            Discount discount = new Discount(p, x + " for " + argument, discountTotal);
            receipt.addDiscount(discount);
        }

    }
}
