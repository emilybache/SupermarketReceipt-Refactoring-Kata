#include "Product.h"

Product::Product(const std::string& name, const ProductUnit& unit) : name(name), unit(unit) {}

std::string Product::getName() const {
    return name;
}

ProductUnit Product::getUnit() const {
    return unit;
}

bool Product::operator==(const Product& rhs) const {
    return name == rhs.name &&
           unit == rhs.unit;
}

bool Product::operator!=(const Product& rhs) const {
    return !(rhs == *this);
}

bool Product::operator<(const Product& rhs) const {
    if (name < rhs.name)
        return true;
    if (rhs.name < name)
        return false;
    return unit < rhs.unit;
}

bool Product::operator>(const Product& rhs) const {
    return rhs < *this;
}

bool Product::operator<=(const Product& rhs) const {
    return !(rhs < *this);
}

bool Product::operator>=(const Product& rhs) const {
    return !(*this < rhs);
}
