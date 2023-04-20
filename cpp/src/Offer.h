#ifndef CPP_OFFER_H
#define CPP_OFFER_H


#include "Product.h"
#include "SpecialOfferType.h"

class Offer {
public:
    Offer() = default;
    Offer(const SpecialOfferType& offerType, const Product& product, double argument);

    SpecialOfferType getOfferType() const;

    Product getProduct() const;

    double getArgument() const;

private:
    SpecialOfferType offerType;
    Product product;
    double argument;

};


#endif //CPP_OFFER_H
