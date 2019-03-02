package dojo.supermarket.model;

public class ThreeForTwo extends Offer {
    public ThreeForTwo(final Product product, final double argument) {
        super(SpecialOfferType.ThreeForTwo, argument);
    }

    @Override
    protected void doHandleOffers(final Receipt receipt, final SupermarketCatalog catalog, final Product p, final ShoppingCart shoppingCart) {
        double quantity = shoppingCart.productQuantities.get(p);
        int quantityAsInt = (int) quantity;
        int THREE_ITEMS = 3;
        int numberOfXs = quantityAsInt / THREE_ITEMS;
        if (quantityAsInt > 2) {
            double unitPrice = catalog.getUnitPrice(p);
            double discountAmount = quantity * unitPrice - ((numberOfXs * 2 * unitPrice) + quantityAsInt % 3 * unitPrice);
            Discount discount = new Discount(p, "3 for 2", discountAmount);
            receipt.addDiscount(discount);
        }
    }
}
