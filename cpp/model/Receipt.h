#ifndef CPP_RECEIPT_H
#define CPP_RECEIPT_H


#include <vector>
#include <ctime>
#include "Discount.h"
#include "ReceiptItem.h"

class Receipt {
public:
    Receipt();

    std::vector<ReceiptItem> getItems() const;

    std::vector<Discount> getDiscounts() const;

    double getTotalPrice() const;

    std::time_t getDate() const;
    void setDate(std::time_t adate);

    void addDiscount(const Discount& discount);

    void addProduct(const Product& product, double quantity, double price, double totalPrice);

private:
    std::vector<ReceiptItem> items;
    std::vector<Discount> discounts;
    std::time_t date;
};


#endif //CPP_RECEIPT_H
