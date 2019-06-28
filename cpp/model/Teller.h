#ifndef CPP_TELLER_H
#define CPP_TELLER_H


#include <map>
#include "SupermarketCatalog.h"
#include "Offer.h"
#include "Receipt.h"
#include "ShoppingCart.h"

class Teller {
public:
    Teller(SupermarketCatalog* catalog);

    void addSpecialOffer(SpecialOfferType offerType, const Product& product, double argument);

    Receipt checksOutArticlesFrom(ShoppingCart theCart);

private:
    SupermarketCatalog* catalog;
    std::map<Product, Offer> offers;
};


#endif //CPP_TELLER_H
