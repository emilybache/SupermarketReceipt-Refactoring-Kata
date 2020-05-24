<?php

namespace Tests;

use PHPUnit\Framework\TestCase;
use Supermarket\Model\{Teller, Product, ProductUnit, ShoppingCart, SpecialOfferType};

class SupermarketTest extends TestCase
{
    public function testTenPercentDiscount()
    {
        // Arrange
        $catalog = new FakeCatalog();
        $toothbrush = new Product('toothbrush', ProductUnit::EACH());
        $catalog->addProduct($toothbrush, 0.99);
        $apples = new Product('apples', ProductUnit::KILO());
        $catalog->addProduct($apples, 1.99);

        $cart = new ShoppingCart();
        $cart->addItemQuantity($apples, 2.5);

        $teller = new Teller($catalog);
        $teller->addSpecialOffer(SpecialOfferType::TEN_PERCENT_DISCOUNT(), $toothbrush, 10.0);

        // Act
        $receipt = $teller->checkoutArticlesFrom($cart);

        // Assert
        self::assertEquals(4.975, $receipt->getTotalPrice());
        self::assertEquals([], $receipt->getDiscounts());
        self::assertCount(1, $receipt->getItems());
        $receiptItem = $receipt->getItems()[0];
        self::assertEquals($apples, $receiptItem->getProduct());
        self::assertEquals(1.99, $receiptItem->getPrice());
        self::assertEquals(2.5 * 1.99, $receiptItem->getTotalPrice());
        self::assertEquals(2.5, $receiptItem->getQuantity());
    }
}
