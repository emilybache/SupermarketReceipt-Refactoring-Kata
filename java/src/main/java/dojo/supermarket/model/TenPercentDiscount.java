package dojo.supermarket.model;

public class TenPercentDiscount extends Offer {
    public TenPercentDiscount(final Product product, final double argument) {
        super(SpecialOfferType.TenPercentDiscount, argument);
    }

    @Override
    protected void doHandleOffers(final Receipt receipt, final SupermarketCatalog catalog, final Product p, final ShoppingCart shoppingCart) {
        double quantity = shoppingCart.productQuantities.get(p);
        double unitPrice = catalog.getUnitPrice(p);
        Discount discount = new Discount(p, argument + "% off", quantity * unitPrice * argument / 100.0);
        receipt.addDiscount(discount);

    }
}
