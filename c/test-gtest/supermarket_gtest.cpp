#include <gtest/gtest.h>

extern "C"
{
#include "supermarket.h"
}

using namespace std;



TEST(Supermarket, TenPercentDiscount)
{
    // ARRANGE
    struct product_t* toothbrush = product_create("toothbrush", Each);
    struct product_t* apples = product_create("apples", Kilo);
    double toothbrush_price = 0.99;
    double apple_price = 1.98;
    struct catalog_t* catalog = catalog_create({toothbrush, apples}, {&toothbrush_price, &apple_price}, 2);
    struct special_offer_t* special_offer = special_offer_create(TenPercentDiscount, toothbrush, 10.0);

    struct teller_t* teller = teller_create(catalog, 2, special_offer);

    double quantity = 2.5;
    struct cart_t* cart = cart_create({apples}, {&quantity}, 1);

    // ACT
    struct receipt_t* receipt = check_out_articles(teller, cart);

    // ASSERT
    EXPECT_NEAR(total_price(receipt), 4.98, 0.01);
}