#include <gtest/gtest.h>
#include <Approvals.h>

#include "SupermarketCatalog.h"
#include "FakeCatalog.h"
#include "ShoppingCart.h"
#include "Teller.h"
#include "ReceiptPrinter.h"

using namespace std;

TEST(SupermarketTest, TenPercentDiscount) {
    // ARRANGE
    SupermarketCatalog *catalog = new FakeCatalog();
    Product toothbrush("toothbrush", ProductUnit::Each);
    catalog->addProduct(toothbrush, 0.99);
    Product apples("apples", ProductUnit::Kilo);
    catalog->addProduct(apples, 1.99);
    Teller teller(catalog);
    teller.addSpecialOffer(SpecialOfferType::TenPercentDiscount, toothbrush, 10.0);

    ShoppingCart cart;
    cart.addItemQuantity(apples, 2.5);

    // ACT
    Receipt receipt = teller.checksOutArticlesFrom(cart);

    // ASSERT
    ASSERT_EQ(4.975, receipt.getTotalPrice());
    ASSERT_TRUE(receipt.getDiscounts().empty());
    ASSERT_EQ(1, receipt.getItems().size());
    ReceiptItem receiptItem = receipt.getItems()[0];
    ASSERT_EQ(apples, receiptItem.getProduct());
    ASSERT_EQ(1.99, receiptItem.getPrice());
    ASSERT_EQ(2.5 * 1.99, receiptItem.getTotalPrice());
    ASSERT_EQ(2.5, receiptItem.getQuantity());

}