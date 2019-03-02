package dojo.supermarket.model;

public class TwoForAmount extends Offer {
    public TwoForAmount(final Product product, final double argument) {
        super(SpecialOfferType.TwoForAmount, argument);
    }

    @Override
    protected void doHandleOffers(final Receipt receipt, final SupermarketCatalog catalog, final Product p, final ShoppingCart shoppingCart) {
        double quantity = shoppingCart.productQuantities.get(p);
        int quantityAsInt = (int) quantity;
        int x = 2;
        if (quantityAsInt >= 2) {
            double unitPrice = catalog.getUnitPrice(p);
            double total = argument * quantityAsInt / x + quantityAsInt % 2 * unitPrice;
            double discountN = unitPrice * quantity - total;
            Discount discount = new Discount(p, "2 for " + argument, discountN);
            receipt.addDiscount(discount);
        }
    }
}
