#ifndef CPP_PRODUCTQUANTITY_H
#define CPP_PRODUCTQUANTITY_H


#include "Product.h"

class ProductQuantity {
private:

public:
    ProductQuantity(const Product& product, double quantity);

    Product getProduct() const;

    double getQuantity() const;

private:
    Product product;
    double quantity;

};


#endif //CPP_PRODUCTQUANTITY_H
