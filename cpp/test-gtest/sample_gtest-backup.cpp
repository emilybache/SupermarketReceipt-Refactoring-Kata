#include <gtest/gtest.h>
#include <Approvals.h>

#include "SupermarketCatalog.h"
#include "FakeCatalog.h"
#include "ShoppingCart.h"
#include "Teller.h"
#include "ReceiptPrinter.h"

using namespace std;


class SupermarketTest : public testing::Test {
protected:
    void SetUp() override {
        catalog = new FakeCatalog();
        teller = new Teller(catalog);
        catalog->addProduct(toothbrush, 0.99);
        catalog->addProduct(apples, 1.99);
        catalog->addProduct(rice, 2.99);
        catalog->addProduct(cherryTomatoes, 0.69);
    }

    void TearDown() override {
        delete teller;
        delete catalog;
    }

    SupermarketCatalog *catalog;
    Teller *teller;
    Product toothbrush{"toothbrush", ProductUnit::Each};
    Product apples{"apples", ProductUnit::Kilo};
    Product rice{"rice", ProductUnit::Each};
    Product cherryTomatoes{"cherry tomato box", ProductUnit::Each};

    ShoppingCart cart;
    ReceiptPrinter printer;
};

TEST_F(SupermarketTest, TenPercentDiscount) {
    // ARRANGE
    teller->addSpecialOffer(SpecialOfferType::TenPercentDiscount, toothbrush, 10.0);
    ShoppingCart cart;
    cart.addItemQuantity(apples, 2.5);

    // ACT
    Receipt receipt = teller->checksOutArticlesFrom(cart);

    // ASSERT
    ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
}

TEST_F(SupermarketTest, EmptyCartCostsNothing) {
    Receipt receipt = teller->checksOutArticlesFrom(cart);
    ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
}

TEST_F(SupermarketTest, OneNormalItem) {
    cart.addItem(toothbrush);
    Receipt receipt = teller->checksOutArticlesFrom(cart);
    ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
}

TEST_F(SupermarketTest, TwoNormalItems) {
    cart.addItem(toothbrush);
    cart.addItem(rice);
    Receipt receipt = teller->checksOutArticlesFrom(cart);
    ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
}

TEST_F(SupermarketTest, BuyTwoGetOneFree) {
    cart.addItem(toothbrush);
    cart.addItem(toothbrush);
    cart.addItem(toothbrush);
    teller->addSpecialOffer(SpecialOfferType::ThreeForTwo, toothbrush, catalog->getUnitPrice(toothbrush));
    Receipt receipt = teller->checksOutArticlesFrom(cart);
    ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
}

TEST_F(SupermarketTest, BuyTwoGetOneFreeButInsufficientInBasket) {
    cart.addItem(toothbrush);
    teller->addSpecialOffer(SpecialOfferType::ThreeForTwo, toothbrush, catalog->getUnitPrice(toothbrush));
    Receipt receipt = teller->checksOutArticlesFrom(cart);
    ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
}

TEST_F(SupermarketTest, BuyFiveGetOneFree) {
    cart.addItem(toothbrush);
    cart.addItem(toothbrush);
    cart.addItem(toothbrush);
    cart.addItem(toothbrush);
    cart.addItem(toothbrush);
    teller->addSpecialOffer(SpecialOfferType::ThreeForTwo, toothbrush, catalog->getUnitPrice(toothbrush));
    Receipt receipt = teller->checksOutArticlesFrom(cart);
    ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
}

TEST_F(SupermarketTest, LooseWeightProduct) {
    cart.addItemQuantity(apples, 0.5);
    Receipt receipt = teller->checksOutArticlesFrom(cart);
    ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
}

TEST_F(SupermarketTest, PercentDiscount) {
    cart.addItem(rice);
    teller->addSpecialOffer(SpecialOfferType::TenPercentDiscount, rice, 10.0);
    Receipt receipt = teller->checksOutArticlesFrom(cart);
    ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
}

TEST_F(SupermarketTest, XForYDiscount) {
    cart.addItem(cherryTomatoes);
    cart.addItem(cherryTomatoes);
    teller->addSpecialOffer(SpecialOfferType::TwoForAmount, cherryTomatoes, 0.99);
    Receipt receipt = teller->checksOutArticlesFrom(cart);
    ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
}

TEST_F(SupermarketTest, XForYDiscountButInsufficientInBasket) {
    cart.addItem(cherryTomatoes);
    teller->addSpecialOffer(SpecialOfferType::TwoForAmount, cherryTomatoes, 0.99);
    Receipt receipt = teller->checksOutArticlesFrom(cart);
    ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
}

TEST_F(SupermarketTest, A5ForYDiscount) {
    cart.addItemQuantity(apples, 5);
    teller->addSpecialOffer(SpecialOfferType::FiveForAmount, apples, 6.99);
    Receipt receipt = teller->checksOutArticlesFrom(cart);
    ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
}

TEST_F(SupermarketTest, A5ForYDiscountWith6) {
    cart.addItemQuantity(apples, 6);
    teller->addSpecialOffer(SpecialOfferType::FiveForAmount, apples, 5.99);
    Receipt receipt = teller->checksOutArticlesFrom(cart);
    ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
}

TEST_F(SupermarketTest, A5ForYDiscountWith16) {
    cart.addItemQuantity(apples, 16);
    teller->addSpecialOffer(SpecialOfferType::FiveForAmount, apples, 7.99);
    Receipt receipt = teller->checksOutArticlesFrom(cart);
    ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
}

TEST_F(SupermarketTest, A5ForYDiscountWith4) {
    cart.addItemQuantity(apples, 4);
    teller->addSpecialOffer(SpecialOfferType::FiveForAmount, apples, 8.99);
    Receipt receipt = teller->checksOutArticlesFrom(cart);
    ApprovalTests::Approvals::verify(printer.printReceipt(receipt));
}