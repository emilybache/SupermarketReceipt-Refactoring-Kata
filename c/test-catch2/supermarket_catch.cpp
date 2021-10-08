#include "ApprovalTests.hpp"
#include "catch2/catch.hpp"

extern "C"
{
#include "supermarket.h"
}


TEST_CASE ("Supermarket") {
    SECTION("Ten percent discount") {
        // ARRANGE
        struct product_t* toothbrush = product_create("toothbrush", Each);
        struct product_t* apples = product_create("apples", Kilo);
        double toothbrush_price = 0.99;
        double apple_price = 1.99;
        struct product_t products[MAX_PRODUCTS] = {*toothbrush, *apples};
        double prices[MAX_PRODUCTS] = {toothbrush_price, apple_price};
        struct catalog_t* catalog = catalog_create(products, prices, 2);
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


