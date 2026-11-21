package dojo.supermarket.model;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Teller {

    private final SupermarketCatalog catalog;
    private final Map<Product, Offer> offers = new HashMap<>();

    public Teller(SupermarketCatalog catalog) {
        this.catalog = catalog;
    }

    public void addSpecialOffer(SpecialOfferType offerType, Product product, double fixedValueOrQuantity) {
        offers.put(product, new Offer(offerType, product, fixedValueOrQuantity));
    }

    public Receipt checksOutArticlesFrom(ShoppingCart theCart) {
        Receipt receipt = new Receipt();
        List<ProductQuantity> productQuantities = theCart.getItems();
        
        addItensToReceipt(productQuantities, receipt);
        theCart.handleOffers(receipt, offers, catalog);

        return receipt;
    }
    
    private void addItensToReceipt(List<ProductQuantity> productQuantities, Receipt receipt) {
        for (ProductQuantity productQtd: productQuantities) {
            Product currentProduct = productQtd.product();
            
            double quantity = productQtd.quantity();
            double unitPrice = catalog.getUnitPrice(currentProduct);
            double price = quantity * unitPrice;
            
            receipt.addProduct(currentProduct, quantity, unitPrice, price);
        }
    }
}
