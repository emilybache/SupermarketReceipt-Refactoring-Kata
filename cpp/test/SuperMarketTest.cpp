

#include "ApprovalTests.hpp"
#include "Catch.hpp"

#include "../model/ReceiptPrinter.h"
#include "../model/SupermarketCatalog.h"
#include "FakeCatalog.h"
#include "../model/ShoppingCart.h"
#include "../model/Teller.h"

TEST_CASE("Discounts", "[Supermarket]")
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

    SECTION("Empty cart costs nothing")
    {
        Receipt receipt = teller.checksOutArticlesFrom(cart);
        ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
    }

    SECTION("One normal item")
    {
        cart.addItem(toothbrush);
        Receipt receipt = teller.checksOutArticlesFrom(cart);
        ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
    }

    SECTION("Two normal items")
    {
        cart.addItem(toothbrush);
        cart.addItem(rice);
        Receipt receipt = teller.checksOutArticlesFrom(cart);
        ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
    }

    SECTION("buy two get one free")
    {
        cart.addItem(toothbrush);
        cart.addItem(toothbrush);
        cart.addItem(toothbrush);
        teller.addSpecialOffer(SpecialOfferType::ThreeForTwo, toothbrush, catalog->getUnitPrice(toothbrush));
        Receipt receipt = teller.checksOutArticlesFrom(cart);
        ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
    }

    SECTION("buy two get one free but insufficient in basket")
    {
        cart.addItem(toothbrush);
        teller.addSpecialOffer(SpecialOfferType::ThreeForTwo, toothbrush, catalog->getUnitPrice(toothbrush));
        Receipt receipt = teller.checksOutArticlesFrom(cart);
        ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
    }
    SECTION("buy five get one free")
    {
        cart.addItem(toothbrush);
        cart.addItem(toothbrush);
        cart.addItem(toothbrush);
        cart.addItem(toothbrush);
        cart.addItem(toothbrush);
        teller.addSpecialOffer(SpecialOfferType::ThreeForTwo, toothbrush, catalog->getUnitPrice(toothbrush));
        Receipt receipt = teller.checksOutArticlesFrom(cart);
        ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
    }


    SECTION("Loose weight product")
    {
        apples.setQuantityType("KG");
        cart.addItemQuantity(apples, 0.5);
        Receipt receipt = teller.checksOutArticlesFrom(cart);
        ApprovalTests::Approvals::verify(printer.printReceipt(receipt));

    }
    SECTION("percent discount")
    {
        cart.addItem(rice);
        teller.addSpecialOffer(SpecialOfferType::TenPercentDiscount, rice, 10.0);
        Receipt receipt = teller.checksOutArticlesFrom(cart);
        ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
    }

    SECTION("x for y discount")
    {
        cart.addItem(cherryTomatoes);
        cart.addItem(cherryTomatoes);
        teller.addSpecialOffer(SpecialOfferType::TwoForAmount, cherryTomatoes, 0.99);
        Receipt receipt = teller.checksOutArticlesFrom(cart);
        ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
    }

    SECTION("x for y discount but insufficient in basket")
    {
        cart.addItem(cherryTomatoes);
        teller.addSpecialOffer(SpecialOfferType::TwoForAmount, cherryTomatoes, 0.99);
        Receipt receipt = teller.checksOutArticlesFrom(cart);
        ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
    }

    SECTION("5 for y discount")
    {
        cart.addItemQuantity(apples, 5);
        teller.addSpecialOffer(SpecialOfferType::FiveForAmount, apples, 6.99);
        Receipt receipt = teller.checksOutArticlesFrom(cart);
        ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
    }

    SECTION("5 for y discount with 6")
    {
        cart.addItemQuantity(apples, 6);
        teller.addSpecialOffer(SpecialOfferType::FiveForAmount, apples, 5.99);
        Receipt receipt = teller.checksOutArticlesFrom(cart);
        ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
    }

    SECTION("5 for y discount with 16")
    {
        cart.addItemQuantity(apples, 16);
        teller.addSpecialOffer(SpecialOfferType::FiveForAmount, apples, 7.99);
        Receipt receipt = teller.checksOutArticlesFrom(cart);
        ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
    }

    SECTION("5 for y discount with 4")
    {
        cart.addItemQuantity(apples, 4);
        teller.addSpecialOffer(SpecialOfferType::FiveForAmount, apples, 8.99);
        Receipt receipt = teller.checksOutArticlesFrom(cart);
        ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
    }
}
