#ifndef CPP_RECEIPT_H
#define CPP_RECEIPT_H


#include <vector>
#include "Discount.h"
#include "ReceiptItem.h"

class Receipt {
public:
    std::vector<ReceiptItem> getItems() const;

    std::vector<Discount> getDiscounts() const;

    double getTotalPrice() const;

    void addDiscount(const Discount& discount);

    void addProduct(const Product& product, double quantity, double price, double totalPrice);

private:
    std::vector<ReceiptItem> items;
    std::vector<Discount> discounts;
};


#endif //CPP_RECEIPT_H
