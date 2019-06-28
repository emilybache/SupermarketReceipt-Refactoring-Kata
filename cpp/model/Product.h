#ifndef CPP_PRODUCT_H
#define CPP_PRODUCT_H


#include <string>
#include "ProductUnit.h"

class Product {
public:
    Product() = default;
    
    Product(const std::string& name, const ProductUnit& unit);

    std::string getName() const;

    ProductUnit getUnit() const;

    bool operator==(const Product& rhs) const;

    bool operator!=(const Product& rhs) const;

    bool operator<(const Product& rhs) const;

    bool operator>(const Product& rhs) const;

    bool operator<=(const Product& rhs) const;

    bool operator>=(const Product& rhs) const;

private:
    std::string name;
    ProductUnit unit;

};


#endif //CPP_PRODUCT_H
