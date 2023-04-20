#include "ApprovalTests.hpp"
#include "doctest/doctest.h"

#include "SupermarketCatalog.h"
#include "FakeCatalog.h"
#include "ShoppingCart.h"
#include "Teller.h"
#include "ReceiptPrinter.h"


TEST_CASE("Discounts")
{
    SupermarketCatalog *catalog = new FakeCatalog();
    Product toothbrush("toothbrush", ProductUnit::Each);
    catalog->addProduct(toothbrush, 0.99);
    Product rice("rice", ProductUnit::Each);
    catalog->addProduct(rice, 2.99);
    Product apples("apples", ProductUnit::Kilo);
    catalog->addProduct(apples, 1.99);
    Product cherryTomatoes("cherry tomato box", ProductUnit::Each);
    catalog->addProduct(cherryTomatoes, 0.69);

    ShoppingCart cart;
    Teller teller(catalog);
    ReceiptPrinter printer = ReceiptPrinter();

    SUBCASE("Empty cart costs nothing") {
        Receipt receipt = teller.checksOutArticlesFrom(cart);
        ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
    }

    SUBCASE("One normal item") {
        cart.addItem(toothbrush);
        Receipt receipt = teller.checksOutArticlesFrom(cart);
        ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
    }

    SUBCASE("Two normal items") {
        cart.addItem(toothbrush);
        cart.addItem(rice);
        Receipt receipt = teller.checksOutArticlesFrom(cart);
        ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
    }

    SUBCASE("buy two get one free") {
        cart.addItem(toothbrush);
        cart.addItem(toothbrush);
        cart.addItem(toothbrush);
        teller.addSpecialOffer(SpecialOfferType::ThreeForTwo, toothbrush, catalog->getUnitPrice(toothbrush));
        Receipt receipt = teller.checksOutArticlesFrom(cart);
        ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
    }

    SUBCASE("buy two get one free but insufficient in basket") {
        cart.addItem(toothbrush);
        teller.addSpecialOffer(SpecialOfferType::ThreeForTwo, toothbrush, catalog->getUnitPrice(toothbrush));
        Receipt receipt = teller.checksOutArticlesFrom(cart);
        ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
    }

    SUBCASE("buy five get one free") {
        cart.addItem(toothbrush);
        cart.addItem(toothbrush);
        cart.addItem(toothbrush);
        cart.addItem(toothbrush);
        cart.addItem(toothbrush);
        teller.addSpecialOffer(SpecialOfferType::ThreeForTwo, toothbrush, catalog->getUnitPrice(toothbrush));
        Receipt receipt = teller.checksOutArticlesFrom(cart);
        ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
    }

    SUBCASE("Loose weight product") {
        cart.addItemQuantity(apples, 0.5);
        Receipt receipt = teller.checksOutArticlesFrom(cart);
        ApprovalTests::Approvals::verify(printer.printReceipt(receipt));

    }

    SUBCASE("percent discount") {
        cart.addItem(rice);
        teller.addSpecialOffer(SpecialOfferType::TenPercentDiscount, rice, 10.0);
        Receipt receipt = teller.checksOutArticlesFrom(cart);
        ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
    }

    SUBCASE("x for y discount") {
        cart.addItem(cherryTomatoes);
        cart.addItem(cherryTomatoes);
        teller.addSpecialOffer(SpecialOfferType::TwoForAmount, cherryTomatoes, 0.99);
        Receipt receipt = teller.checksOutArticlesFrom(cart);
        ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
    }

    SUBCASE("x for y discount but insufficient in basket") {
        cart.addItem(cherryTomatoes);
        teller.addSpecialOffer(SpecialOfferType::TwoForAmount, cherryTomatoes, 0.99);
        Receipt receipt = teller.checksOutArticlesFrom(cart);
        ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
    }

    SUBCASE("5 for y discount") {
        cart.addItemQuantity(apples, 5);
        teller.addSpecialOffer(SpecialOfferType::FiveForAmount, apples, 6.99);
        Receipt receipt = teller.checksOutArticlesFrom(cart);
        ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
    }

    SUBCASE("5 for y discount with 6") {
        cart.addItemQuantity(apples, 6);
        teller.addSpecialOffer(SpecialOfferType::FiveForAmount, apples, 5.99);
        Receipt receipt = teller.checksOutArticlesFrom(cart);
        ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
    }

    SUBCASE("5 for y discount with 16") {
        cart.addItemQuantity(apples, 16);
        teller.addSpecialOffer(SpecialOfferType::FiveForAmount, apples, 7.99);
        Receipt receipt = teller.checksOutArticlesFrom(cart);
        ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
    }

    SUBCASE("5 for y discount with 4") {
        cart.addItemQuantity(apples, 4);
        teller.addSpecialOffer(SpecialOfferType::FiveForAmount, apples, 8.99);
        Receipt receipt = teller.checksOutArticlesFrom(cart);
        ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
    }
    delete catalog;
}



