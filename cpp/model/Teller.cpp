#include "Teller.h"

Teller::Teller(SupermarketCatalog *catalog) : catalog(catalog) {}

void Teller::addSpecialOffer(SpecialOfferType offerType, const Product& product, double argument) {
    offers[product] = Offer(offerType, product, argument);
}

Receipt Teller::checksOutArticlesFrom(ShoppingCart theCart) {
    Receipt receipt{};
    std::vector<ProductQuantity> productQuantities = theCart.getItems();
    for (const auto& pq: productQuantities) {
        Product p = pq.getProduct();
        double quantity = pq.getQuantity();
        double unitPrice = catalog->getUnitPrice(p);
        double price = quantity * unitPrice;
        receipt.addProduct(p, quantity, unitPrice, price);
    }
    theCart.handleOffers(receipt, offers, catalog);

    return receipt;
}
