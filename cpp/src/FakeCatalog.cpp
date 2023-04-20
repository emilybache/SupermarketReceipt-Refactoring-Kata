#include "FakeCatalog.h"

void FakeCatalog::addProduct(const Product& product, double price) {
    products[product.getName()] = product;
    prices[product.getName()] = price;
}

double FakeCatalog::getUnitPrice(const Product& product) {
    return prices[product.getName()];
}

FakeCatalog::~FakeCatalog() = default;
