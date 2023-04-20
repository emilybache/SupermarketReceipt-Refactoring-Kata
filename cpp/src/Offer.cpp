#include "Offer.h"

Offer::Offer(const SpecialOfferType& offerType, const Product& product, double argument)
        : offerType(offerType), product(product), argument(argument) {}

SpecialOfferType Offer::getOfferType() const {
        return offerType;
}

Product Offer::getProduct() const {
        return product;
}

double Offer::getArgument() const {
        return argument;
}
