#include "ApprovalTests.hpp"
#include "catch2/catch.hpp"
#include <string>

extern "C"
{
#include "supermarket.h"
#include "printer.h"
}

using namespace std;

TEST_CASE ("Supermarket") {
    struct product_t* toothbrush = product_create("toothbrush", Each);
    struct product_t* apples = product_create("apples", Kilo);
    struct product_t* rice = product_create("rice", Each);
    struct product_t* cherry_tomtoes = product_create("cherry tomato box", Each);
    double toothbrush_price = 0.99;
    double apple_price = 1.99;
    double rice_price = 2.99;
    double tomato_price = 0.69;
    struct product_t products[MAX_PRODUCTS] = {*toothbrush, *apples, *rice, *cherry_tomtoes};
    double prices[MAX_PRODUCTS] = {toothbrush_price, apple_price, rice_price, tomato_price};
    struct catalog_t* catalog = catalog_create(products, prices, 4);

    char buffer[MAX_PRINT_LENGTH];

    SECTION("Empty cart costs nothing") {

        struct teller_t* teller = teller_create(catalog, 4, nullptr);

        struct cart_t* cart = cart_create({}, {}, 0);

        struct receipt_t* receipt = check_out_articles(teller, cart);

        print_receipt(buffer, receipt);
        ApprovalTests::Approvals::verify(string(buffer));
    }

    SECTION("Ten percent discount") {
        struct special_offer_t* special_offer = special_offer_create(TenPercentDiscount, toothbrush, 10.0);

        struct teller_t* teller = teller_create(catalog, 2, special_offer);

        struct product_t cart_products[] = {*apples};
        double cart_quantities[] = {2.5};
        struct cart_t* cart = cart_create(cart_products, cart_quantities, 1);

        // ACT
        struct receipt_t* receipt = check_out_articles(teller, cart);

        // ASSERT
        CHECK(total_price(receipt) == 4.975);
        CHECK(0 == receipt->discountCount);
        CHECK(1 == receipt->itemCount);
        struct receipt_item_t item = receipt->items[0];
        CHECK(1.99 == item.price);
        CHECK(2.5*1.99 == item.totalPrice);
        CHECK(2.5 == item.quantity);
    }

}


