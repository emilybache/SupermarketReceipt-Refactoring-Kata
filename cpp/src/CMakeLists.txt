set(SRC_LIB_NAME src)
set(SOURCES Discount.cpp
        Discount.h Product.cpp Product.h
        Offer.cpp Offer.h SpecialOfferType.h
        ProductUnit.h ProductQuantity.cpp ProductQuantity.h
        ReceiptItem.cpp ReceiptItem.h Receipt.cpp Receipt.h
        ShoppingCart.cpp ShoppingCart.h SupermarketCatalog.h Teller.cpp
        Teller.h ReceiptPrinter.h ReceiptPrinter.cpp FakeCatalog.cpp)
add_library(${SRC_LIB_NAME} ${SOURCES})
target_sources(${SRC_LIB_NAME} PRIVATE ${SOURCES})
target_include_directories(${SRC_LIB_NAME} PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
