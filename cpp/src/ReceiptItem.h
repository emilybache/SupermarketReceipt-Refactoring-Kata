#ifndef CPP_RECEIPTITEM_H
#define CPP_RECEIPTITEM_H


#include "Product.h"

class ReceiptItem {
public:
    ReceiptItem(const Product& product, double quantity, double price, double totalPrice);
    Product getProduct() const;

    double getPrice() const;

    double getTotalPrice() const;

    double getQuantity() const;

    bool operator==(const ReceiptItem& rhs) const;

    bool operator!=(const ReceiptItem& rhs) const;

private:
    Product product;
    double price;
    double totalPrice;
    double quantity;

};


#endif //CPP_RECEIPTITEM_H
