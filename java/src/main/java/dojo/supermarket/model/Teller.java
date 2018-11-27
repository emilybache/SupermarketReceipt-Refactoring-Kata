package dojo.supermarket.model;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Teller {

    private final SupermarketCatalog catalog;
    private Map<Product, Offer> offers = new HashMap<>();

    public Teller(SupermarketCatalog catalog) {
        this.catalog = catalog;
    }

    public void addSpecialOffer(SpecialOfferType offerType, Product product, double argument) {
        Offer offer = createSpecialOffer(offerType, product, argument);
        this.offers.put(product, offer);
    }

    private Offer createSpecialOffer(SpecialOfferType offerType, Product product, double argument) {
        switch (offerType) {
            case ThreeForTwo:
                return new Offer(SpecialOfferType.ThreeForTwo, product, argument);
            case TenPercentDiscount:
                return new Offer(SpecialOfferType.TenPercentDiscount, product, argument);
            case TwoForAmount:
                return new Offer(SpecialOfferType.TwoForAmount, product, argument);
            case FiveForAmount:
                return new Offer(SpecialOfferType.FiveForAmount, product, argument);
        }
        throw new IllegalArgumentException("unreachable");
    }

    public Receipt checksOutArticlesFrom(ShoppingCart theCart) {
        Receipt receipt = new Receipt();
        List<ProductQuantity> productQuantities = theCart.getItems();
        for (ProductQuantity pq: productQuantities) {
            Product p = pq.getProduct();
            double quantity = pq.getQuantity();
            double unitPrice = this.catalog.getUnitPrice(p);
            double price = quantity * unitPrice;
            receipt.addProduct(p, quantity, unitPrice, price);
        }
        theCart.handleOffers(receipt, this.offers, this.catalog);

        return receipt;
    }

}
