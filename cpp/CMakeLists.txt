cmake_minimum_required(VERSION 3.5)
project(SuperMarket)

enable_testing()
find_package(GTest)

include(ExternalProject)
if(NOT ${GTEST_FOUND}) # Download gtest if not installed
    message("No system gtest use external project")
    ExternalProject_Add(googletest
            GIT_REPOSITORY    https://github.com/google/googletest.git
            GIT_TAG           master
            SOURCE_DIR        "${CMAKE_BINARY_DIR}/googletest-src"
            BINARY_DIR        "${CMAKE_BINARY_DIR}/googletest-build"
            CONFIGURE_COMMAND ""
            BUILD_COMMAND     ""
            INSTALL_COMMAND   ""
            TEST_COMMAND      ""
            )
    add_subdirectory(${CMAKE_BINARY_DIR}/googletest-src
            ${CMAKE_BINARY_DIR}/googletest-build
            EXCLUDE_FROM_ALL)
    set(GTEST_BOTH_LIBRARIES gtest gtest_main)
endif()

include_directories(${GTEST_INCLUDE_DIRS})
include_directories(${CMAKE_BINARY_DIR}/test)


set(CMAKE_CXX_STANDARD 14)

add_executable(SuperMarket test/main.cpp model/Discount.cpp
        model/Discount.h model/Product.cpp model/Product.h
        model/Offer.cpp model/Offer.h model/SpecialOfferType.h
        model/ProductUnit.h model/ProductQuantity.cpp model/ProductQuantity.h
        model/ReceiptItem.cpp model/ReceiptItem.h model/Receipt.cpp model/Receipt.h
        model/ShoppingCart.cpp model/ShoppingCart.h model/SupermarketCatalog.h model/Teller.cpp
        model/Teller.h ReceiptPrinter.cpp ReceiptPrinter.h test/FakeCatalog.cpp
        test/FakeCatalog.h test/SuperMarketTest.cpp)
target_link_libraries(SuperMarket ${GTEST_BOTH_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})
add_test(NAME SuperMarket COMMAND SuperMarket)