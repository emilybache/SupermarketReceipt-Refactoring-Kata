#ifndef CPP_SUPERMARKETTEST_H
#define CPP_SUPERMARKETTEST_H

#include <gtest/gtest.h>
#include "../ReceiptPrinter.h"
#include "../model/SupermarketCatalog.h"
#include "FakeCatalog.h"
#include "../model/ShoppingCart.h"
#include "../model/Teller.h"

#include "../third_party/ApprovalTests.v.5.1.0.hpp"


TEST(SuperMarketTest, foo) {
        SupermarketCatalog* catalog = new FakeCatalog();
        Product toothbrush("toothbrush", ProductUnit::Each);
        catalog->addProduct(toothbrush, 0.99);
        Product apples("apples", ProductUnit::Kilo);
        catalog->addProduct(apples, 1.99);

        ShoppingCart cart;
        cart.addItemQuantity(apples, 2.5);

        Teller teller(catalog);
        teller.addSpecialOffer(SpecialOfferType::TenPercentDiscount, toothbrush, 10.0);

        Receipt receipt = teller.checksOutArticlesFrom(cart);

        ASSERT_EQ(4.975, receipt.getTotalPrice());

        ASSERT_EQ(0, receipt.getDiscounts().size());
        ASSERT_EQ(1, receipt.getItems().size());
        ReceiptItem receiptItem = receipt.getItems()[0];
        ASSERT_EQ(apples, receiptItem.getProduct());
        ASSERT_EQ(1.99, receiptItem.getPrice());
        ASSERT_EQ(2.5 * 1.99, receiptItem.getTotalPrice());
        ASSERT_EQ(2.5, receiptItem.getQuantity());

    }


#endif //CPP_SUPERMARKETTEST_H
