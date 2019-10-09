#include "ProductQuantity.h"

ProductQuantity::ProductQuantity(const Product& product, double quantity) : product(product), quantity(quantity) {}

Product ProductQuantity::getProduct() const {
    return product;
}

double ProductQuantity::getQuantity() const {
    return quantity;
}
