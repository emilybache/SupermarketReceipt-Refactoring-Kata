//
// Created by sdargo on 2/4/19.
//

#include "FakeCatalog.h"

void FakeCatalog::addProduct(const Product& product, double price) {
    products[product.getName()] = product;
    prices[product.getName()] = price;
}

double FakeCatalog::getUnitPrice(const Product& product) {
    return prices[product.getName()];
}
