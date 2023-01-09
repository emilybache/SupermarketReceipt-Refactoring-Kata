<?php

declare(strict_types=1);

namespace Tests;

use PHPUnit\Framework\TestCase;
use Supermarket\Model\Product;
use Supermarket\Model\ProductUnit;
use Supermarket\Model\ShoppingCart;
use Supermarket\Model\SpecialOfferType;
use Supermarket\Model\Teller;

class SupermarketTest extends TestCase
{
    public function testTenPercentDiscount(): void
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
        self::assertSame(4.975, $receipt->getTotalPrice());
        self::assertSame([], $receipt->getDiscounts());
        self::assertCount(1, $receipt->getItems());
        $receiptItem = $receipt->getItems()[0];
        self::assertSame($apples, $receiptItem->getProduct());
        self::assertSame(1.99, $receiptItem->getPrice());
        self::assertSame(2.5 * 1.99, $receiptItem->getTotalPrice());
        self::assertSame(2.5, $receiptItem->getQuantity());
    }
}
