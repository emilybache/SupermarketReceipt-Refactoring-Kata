#include "Discount.h"
#include "Product.h"

Discount::Discount(const std::string& description, double discountAmount, const Product& product)
        : description(description), discountAmount(discountAmount), product(product) {}

std::string Discount::getDescription() const {
    return description;
}

double Discount::getDiscountAmount() const {
    return discountAmount;
}

Product Discount::getProduct() const {
    return product;
}
