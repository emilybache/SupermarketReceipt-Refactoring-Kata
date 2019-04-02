#include "ReceiptItem.h"

ReceiptItem::ReceiptItem(const Product& product, double quantity, double price, double totalPrice)
    : product(product), price(price), totalPrice(totalPrice), quantity(quantity) {}

Product ReceiptItem::getProduct() const {
    return product;
}

double ReceiptItem::getPrice() const {
    return price;
}

double ReceiptItem::getTotalPrice() const {
    return totalPrice;
}

double ReceiptItem::getQuantity() const {
    return quantity;
}

bool ReceiptItem::operator==(const ReceiptItem& rhs) const {
    return product == rhs.product &&
           price == rhs.price &&
           totalPrice == rhs.totalPrice &&
           quantity == rhs.quantity;
}

bool ReceiptItem::operator!=(const ReceiptItem& rhs) const {
    return !(rhs == *this);
}
