<?php

declare(strict_types=1);

namespace Tests;

use ApprovalTests\Approvals;
use PHPUnit\Framework\TestCase;
use Supermarket\Model\Product;
use Supermarket\Model\ProductUnit;
use Supermarket\Model\ShoppingCart;
use Supermarket\Model\SpecialOfferType;
use Supermarket\Model\Teller;
use Supermarket\ReceiptPrinter;

class SupermarketTest extends TestCase
{
    private FakeCatalog $catalog;

    private Product $toothbrush;

    private Product $rice;

    private Product $apples;

    private Product $cherryTomatoes;

    private ShoppingCart $cart;

    private Teller $teller;

    private ReceiptPrinter $printer;

    protected function setUp(): void
    {
        parent::setUp();

        $this->catalog = new FakeCatalog();
        $this->cart = new ShoppingCart();
        $this->teller = new Teller($this->catalog);

        $this->toothbrush = new Product('toothbrush', ProductUnit::EACH());
        $this->catalog->addProduct($this->toothbrush, 0.99);
        $this->rice = new Product('rice', ProductUnit::EACH());
        $this->catalog->addProduct($this->rice, 2.99);
        $this->apples = new Product('apples', ProductUnit::KILO());
        $this->catalog->addProduct($this->apples, 1.99);
        $this->cherryTomatoes = new Product('cherry tomato box', ProductUnit::EACH());
        $this->catalog->addProduct($this->cherryTomatoes, 0.69);

        $this->printer = new ReceiptPrinter(40);
    }

    public function testAnEmptyShoppingCartShouldCostNothing(): void
    {
        Approvals::verifyString($this->printReceipt());
    }

    public function testOneNormalItem(): void
    {
        $this->cart->addItem($this->toothbrush);

        Approvals::verifyString($this->printReceipt());
    }

    public function testTwoNormalItems(): void
    {
        $this->cart->addItem($this->toothbrush);
        $this->cart->addItem($this->rice);

        Approvals::verifyString($this->printReceipt());
    }

    public function testBuyTwoGetOneFree(): void
    {
        $this->cart->addItem($this->toothbrush);
        $this->cart->addItem($this->toothbrush);
        $this->cart->addItem($this->toothbrush);
        $this->teller->addSpecialOffer(
            SpecialOfferType::THREE_FOR_TWO(),
            $this->toothbrush,
            $this->catalog->getUnitPrice($this->toothbrush)
        );

        Approvals::verifyString($this->printReceipt());
    }

    public function testBuyTwoGetOneFreeButInsufficientInBasket(): void
    {
        $this->cart->addItem($this->toothbrush);
        $this->teller->addSpecialOffer(
            SpecialOfferType::THREE_FOR_TWO(),
            $this->toothbrush,
            $this->catalog->getUnitPrice($this->toothbrush)
        );

        Approvals::verifyString($this->printReceipt());
    }

    public function testBuyFiveGetOneFree(): void
    {
        $this->cart->addItem($this->toothbrush);
        $this->cart->addItem($this->toothbrush);
        $this->cart->addItem($this->toothbrush);
        $this->cart->addItem($this->toothbrush);
        $this->cart->addItem($this->toothbrush);
        $this->teller->addSpecialOffer(
            SpecialOfferType::THREE_FOR_TWO(),
            $this->toothbrush,
            $this->catalog->getUnitPrice($this->toothbrush)
        );

        Approvals::verifyString($this->printReceipt());
    }

    public function testLooseWeightProduct(): void
    {
        $this->cart->addItemQuantity($this->apples, 0.5);

        Approvals::verifyString($this->printReceipt());
    }

    public function testPercentDiscount(): void
    {
        $this->cart->addItem($this->rice);
        $this->teller->addSpecialOffer(SpecialOfferType::TEN_PERCENT_DISCOUNT(), $this->rice, 10.0);

        Approvals::verifyString($this->printReceipt());
    }

    public function testXForYDiscount(): void
    {
        $this->cart->addItem($this->cherryTomatoes);
        $this->cart->addItem($this->cherryTomatoes);
        $this->teller->addSpecialOffer(SpecialOfferType::TWO_FOR_AMOUNT(), $this->cherryTomatoes, 0.99);

        Approvals::verifyString($this->printReceipt());
    }

    public function testXForYDiscountWithInsufficientInBasket(): void
    {
        $this->cart->addItem($this->cherryTomatoes);
        $this->teller->addSpecialOffer(SpecialOfferType::TWO_FOR_AMOUNT(), $this->cherryTomatoes, 0.99);

        Approvals::verifyString($this->printReceipt());
    }

    public function testFiveForYDiscount(): void
    {
        $this->cart->addItemQuantity($this->apples, 5);
        $this->teller->addSpecialOffer(SpecialOfferType::FIVE_FOR_AMOUNT(), $this->apples, 6.99);

        Approvals::verifyString($this->printReceipt());
    }

    public function testFiveForYDiscountWithSix(): void
    {
        $this->cart->addItemQuantity($this->apples, 6);
        $this->teller->addSpecialOffer(SpecialOfferType::FIVE_FOR_AMOUNT(), $this->apples, 6.99);

        Approvals::verifyString($this->printReceipt());
    }

    public function testFiveForYDiscountWithSixteen(): void
    {
        $this->cart->addItemQuantity($this->apples, 16);
        $this->teller->addSpecialOffer(SpecialOfferType::FIVE_FOR_AMOUNT(), $this->apples, 6.99);

        Approvals::verifyString($this->printReceipt());
    }

    public function testFiveForYDiscountWithFour(): void
    {
        $this->cart->addItemQuantity($this->apples, 4);
        $this->teller->addSpecialOffer(SpecialOfferType::FIVE_FOR_AMOUNT(), $this->apples, 6.99);

        Approvals::verifyString($this->printReceipt());
    }

    private function printReceipt(): string
    {
        $receipt = $this->teller->checkoutArticlesFrom($this->cart);
        return $this->printer->printReceipt($receipt);
    }
}
