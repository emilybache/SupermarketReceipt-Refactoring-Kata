

#include "ApprovalTests.hpp"
#include "Catch.hpp"

#include "../model/ReceiptPrinter.h"
#include "../model/SupermarketCatalog.h"
#include "FakeCatalog.h"
#include "../model/ShoppingCart.h"
#include "../model/Teller.h"

TEST_CASE("TenPercentDiscount", "[Supermarket]")
{
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
}
