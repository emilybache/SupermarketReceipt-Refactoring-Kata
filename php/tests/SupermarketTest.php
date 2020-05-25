<?php

declare(strict_types=1);

namespace Tests;

use PHPUnit\Framework\TestCase;
use Supermarket\model\Product;
use Supermarket\model\ProductUnit;
use Supermarket\model\ReceiptItem;
use Supermarket\model\ShoppingCart;
use Supermarket\model\SpecialOfferType;
use Supermarket\model\SupermarketCatalog;
use Supermarket\model\Teller;

class SupermarketTest extends TestCase
{
    // Todo: test all kinds of discounts are applied properly

    public function testTenPercentDiscount(): void
    {
        /** @var SupermarketCatalog $catalog */
        $catalog = new FakeCatalog();
        $toothbrush = new Product('toothbrush', ProductUnit::EACH);

        $catalog->addProduct($toothbrush, 0.99);

        $apples = new Product('apples', ProductUnit::KILO);

        $catalog->addProduct($apples, 1.99);

        $teller = new Teller($catalog);

        $teller->addSpecialOffer(SpecialOfferType::TEN_PERCENT_DISCOUNT, $toothbrush, 10.0);

        $cart = new ShoppingCart();

        $cart->addItemQuantity($apples, 2.5);

        // ACT
        $receipt = $teller->checksOutArticlesFrom($cart);

        // ASSERT
        $this->assertSame(4.975, $receipt->getTotalPrice());
        $this->assertSame([], $receipt->getDiscounts());
        $this->assertSame(1, count($receipt->getItems()));

        /** @var ReceiptItem $receiptItem */
        $receiptItem = $receipt->getItems()[0];
        $this->assertSame('apples', $receiptItem->getProduct()->getName());
        $this->assertSame(1.99, $receiptItem->getPrice());
        $this->assertSame(2.5 * 1.99, $receiptItem->getTotalPrice());
        $this->assertSame(2.5, $receiptItem->getQuantity());
    }
}
