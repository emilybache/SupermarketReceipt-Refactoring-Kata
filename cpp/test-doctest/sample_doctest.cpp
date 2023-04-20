#include "ApprovalTests.hpp"
#include "doctest/doctest.h"

#include "SupermarketCatalog.h"
#include "FakeCatalog.h"
#include "ShoppingCart.h"
#include "Teller.h"

TEST_CASE("TenPercentDiscount")
{
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
    REQUIRE(4.975 == receipt.getTotalPrice());
    REQUIRE(receipt.getDiscounts().empty());
    REQUIRE(1 == receipt.getItems().size());
    ReceiptItem receiptItem = receipt.getItems()[0];
    REQUIRE(apples == receiptItem.getProduct());
    REQUIRE(1.99 == receiptItem.getPrice());
    REQUIRE(2.5 * 1.99 == receiptItem.getTotalPrice());
    REQUIRE(2.5 == receiptItem.getQuantity());

}


