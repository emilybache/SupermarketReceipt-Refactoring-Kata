#ifndef CPP_SUPERMARKETCATALOG_H
#define CPP_SUPERMARKETCATALOG_H


#include "Product.h"

class SupermarketCatalog {
public:
    SupermarketCatalog() = default;
    virtual ~SupermarketCatalog() = default;
    virtual void addProduct(const Product& product, double price) = 0;
    virtual double getUnitPrice(const Product& product) = 0;
};


#endif //CPP_SUPERMARKETCATALOG_H
