#ifndef CPP_SUPERMARKETTEST_H
#define CPP_SUPERMARKETTEST_H

#include <gtest/gtest.h>
#include "../ReceiptPrinter.h"
#include "../model/SupermarketCatalog.h"
#include "FakeCatalog.h"
#include "../model/ShoppingCart.h"
#include "../model/Teller.h"

#include "../approval/ApprovalTests.v.5.0.0.hpp"


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
        ReceiptPrinter rp = ReceiptPrinter();
        
        ApprovalTests::Approvals::verify(rp.printReceipt(receipt));

        // Todo: complete this test
    }


#endif //CPP_SUPERMARKETTEST_H
