#include "ApprovalTests.hpp"
#include "catch2/catch.hpp"
#include <string>

extern "C"
{
#include "supermarket.h"
#include "printer.h"
}

using namespace std;

#define PRODUCT_COUNT 4

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
    struct catalog_t* catalog = catalog_create(products, prices, PRODUCT_COUNT);

    char buffer[MAX_PRINT_LENGTH];

    SECTION("Empty cart costs nothing") {
        struct teller_t* teller = teller_create(catalog, PRODUCT_COUNT, nullptr);
        struct cart_t* cart = cart_create({}, {}, 0);

        struct receipt_t* receipt = check_out_articles(teller, cart);

        print_receipt(buffer, receipt);
        ApprovalTests::Approvals::verify(string(buffer));
    }

    SECTION("One normal item")
    {
        struct teller_t* teller = teller_create(catalog, PRODUCT_COUNT, nullptr);
        struct product_t cart_products[] = {*toothbrush};
        double cart_quantities[] = {1};
        struct cart_t* cart = cart_create(cart_products, cart_quantities, 1);

        struct receipt_t* receipt = check_out_articles(teller, cart);

        print_receipt(buffer, receipt);
        ApprovalTests::Approvals::verify(string(buffer));
    }

    SECTION("Two normal items")
    {
        struct teller_t* teller = teller_create(catalog, PRODUCT_COUNT, nullptr);
        struct product_t cart_products[] = {*toothbrush, *rice};
        double cart_quantities[] = {2, 1};
        struct cart_t* cart = cart_create(cart_products, cart_quantities, 2);

        struct receipt_t* receipt = check_out_articles(teller, cart);

        print_receipt(buffer, receipt);
        ApprovalTests::Approvals::verify(string(buffer));
    }

    SECTION("buy two get one free")
    {
        struct special_offer_t* three_for_two = special_offer_create(ThreeForTwo, toothbrush, 10);
        struct teller_t* teller = teller_create(catalog, PRODUCT_COUNT, three_for_two);
        struct product_t cart_products[] = {*toothbrush};
        double cart_quantities[] = {3};
        struct cart_t* cart = cart_create(cart_products, cart_quantities, 1);

        struct receipt_t* receipt = check_out_articles(teller, cart);

        print_receipt(buffer, receipt);
        ApprovalTests::Approvals::verify(string(buffer));
    }

    SECTION("buy two get one free but insufficient in basket")
    {
        struct special_offer_t* three_for_two = special_offer_create(ThreeForTwo, toothbrush, 10);
        struct teller_t* teller = teller_create(catalog, PRODUCT_COUNT, three_for_two);
        struct product_t cart_products[] = {*toothbrush};
        double cart_quantities[] = {1};
        struct cart_t* cart = cart_create(cart_products, cart_quantities, 1);

        struct receipt_t* receipt = check_out_articles(teller, cart);

        print_receipt(buffer, receipt);
        ApprovalTests::Approvals::verify(string(buffer));
    }

    SECTION("buy five get one free")
    {
        struct special_offer_t* three_for_two = special_offer_create(ThreeForTwo, toothbrush, 10);
        struct teller_t* teller = teller_create(catalog, PRODUCT_COUNT, three_for_two);
        struct product_t cart_products[] = {*toothbrush};
        double cart_quantities[] = {5};
        struct cart_t* cart = cart_create(cart_products, cart_quantities, 1);

        struct receipt_t* receipt = check_out_articles(teller, cart);

        print_receipt(buffer, receipt);
        ApprovalTests::Approvals::verify(string(buffer));
    }

    SECTION("Loose weight product")
    {
        struct teller_t* teller = teller_create(catalog, PRODUCT_COUNT, nullptr);
        struct product_t cart_products[] = {*apples};
        double cart_quantities[] = {0.5};
        struct cart_t* cart = cart_create(cart_products, cart_quantities, 1);

        struct receipt_t* receipt = check_out_articles(teller, cart);

        print_receipt(buffer, receipt);
        ApprovalTests::Approvals::verify(string(buffer));

    }

    SECTION("percent discount")
    {
        struct special_offer_t* special_offer = special_offer_create(TenPercentDiscount, rice, 10.0);
        struct teller_t* teller = teller_create(catalog, PRODUCT_COUNT, special_offer);
        struct product_t cart_products[] = {*rice};
        double cart_quantities[] = {2};
        struct cart_t* cart = cart_create(cart_products, cart_quantities, 1);

        struct receipt_t* receipt = check_out_articles(teller, cart);

        print_receipt(buffer, receipt);
        ApprovalTests::Approvals::verify(string(buffer));
    }

    SECTION("x for y discount")
    {
        struct special_offer_t* special_offer = special_offer_create(TwoForAmount, cherry_tomtoes, 0.99);
        struct teller_t* teller = teller_create(catalog, PRODUCT_COUNT, special_offer);
        struct product_t cart_products[] = {*cherry_tomtoes};
        double cart_quantities[] = {2};
        struct cart_t* cart = cart_create(cart_products, cart_quantities, 1);

        struct receipt_t* receipt = check_out_articles(teller, cart);

        print_receipt(buffer, receipt);
        ApprovalTests::Approvals::verify(string(buffer));
    }

    SECTION("x for y discount but insufficient in basket")
    {
        struct special_offer_t* special_offer = special_offer_create(TwoForAmount, cherry_tomtoes, 0.99);
        struct teller_t* teller = teller_create(catalog, PRODUCT_COUNT, special_offer);
        struct product_t cart_products[] = {*cherry_tomtoes};
        double cart_quantities[] = {1};
        struct cart_t* cart = cart_create(cart_products, cart_quantities, 1);

        struct receipt_t* receipt = check_out_articles(teller, cart);

        print_receipt(buffer, receipt);
        ApprovalTests::Approvals::verify(string(buffer));
    }

   SECTION("5 for y discount")
   {
        struct special_offer_t* special_offer = special_offer_create(FiveForAmount, apples, 6.99);
        struct teller_t* teller = teller_create(catalog, PRODUCT_COUNT, special_offer);
        struct product_t cart_products[] = {*apples};
        double cart_quantities[] = {5};
        struct cart_t* cart = cart_create(cart_products, cart_quantities, 1);

        struct receipt_t* receipt = check_out_articles(teller, cart);

        print_receipt(buffer, receipt);
        ApprovalTests::Approvals::verify(string(buffer));

   }

  SECTION("5 for y discount with 6")
  {
        struct special_offer_t* special_offer = special_offer_create(FiveForAmount, apples, 5.99);
        struct teller_t* teller = teller_create(catalog, PRODUCT_COUNT, special_offer);
        struct product_t cart_products[] = {*apples};
        double cart_quantities[] = {6};
        struct cart_t* cart = cart_create(cart_products, cart_quantities, 1);

        struct receipt_t* receipt = check_out_articles(teller, cart);

        print_receipt(buffer, receipt);
        ApprovalTests::Approvals::verify(string(buffer));

  }

     SECTION("5 for y discount with 16")
     {
        struct special_offer_t* special_offer = special_offer_create(FiveForAmount, apples, 7.99);
        struct teller_t* teller = teller_create(catalog, PRODUCT_COUNT, special_offer);
        struct product_t cart_products[] = {*apples};
        double cart_quantities[] = {16};
        struct cart_t* cart = cart_create(cart_products, cart_quantities, 1);

        struct receipt_t* receipt = check_out_articles(teller, cart);

        print_receipt(buffer, receipt);
        ApprovalTests::Approvals::verify(string(buffer));
     }

     SECTION("5 for y discount with 4")
     {
        struct special_offer_t* special_offer = special_offer_create(FiveForAmount, apples, 8.99);
        struct teller_t* teller = teller_create(catalog, PRODUCT_COUNT, special_offer);
        struct product_t cart_products[] = {*apples};
        double cart_quantities[] = {4};
        struct cart_t* cart = cart_create(cart_products, cart_quantities, 1);

        struct receipt_t* receipt = check_out_articles(teller, cart);

        print_receipt(buffer, receipt);
        ApprovalTests::Approvals::verify(string(buffer));
     }

    memset(buffer, 0, sizeof buffer);
}


