cmake_minimum_required(VERSION 3.8)
project(Supermarket.model CXX)
set(SOURCE_FILES Discount.cpp
        Discount.h Product.cpp Product.h
        Offer.cpp Offer.h SpecialOfferType.h
        ProductUnit.h ProductQuantity.cpp ProductQuantity.h
        ReceiptItem.cpp ReceiptItem.h Receipt.cpp Receipt.h
        ShoppingCart.cpp ShoppingCart.h SupermarketCatalog.h Teller.cpp
        Teller.h ReceiptPrinter.h ReceiptPrinter.cpp)

add_library(${PROJECT_NAME} ${SOURCE_FILES})
target_include_directories(${PROJECT_NAME}
        INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/..)