<?php

namespace Tests;

use ApprovalTests\Approvals;
use PHPUnit\Framework\TestCase;
use Supermarket\Model\{Receipt, SpecialOfferType, Teller, Product, ProductUnit, ShoppingCart};
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

    public function setUp(): void
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

    /** @test */
    public function anEmptyShoppingCartShouldCostNothing()
    {
        Approvals::verifyString($this->printReceipt());
    }

    /** @test */
    public function oneNormalItem()
    {
        $this->cart->addItem($this->toothbrush);

        Approvals::verifyString($this->printReceipt());
    }

    /** @test */
    public function twoNormalItems()
    {
        $this->cart->addItem($this->toothbrush);
        $this->cart->addItem($this->rice);

        Approvals::verifyString($this->printReceipt());
    }

    /** @test */
    public function buyTwoGetOneFree()
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

    /** @test */
    public function buyTwoGetOneFreeButInsufficientInBasket()
    {
        $this->cart->addItem($this->toothbrush);
        $this->teller->addSpecialOffer(
            SpecialOfferType::THREE_FOR_TWO(),
            $this->toothbrush,
            $this->catalog->getUnitPrice($this->toothbrush)
        );

        Approvals::verifyString($this->printReceipt());
    }

    /** @test */
    public function buyFiveGetOneFree()
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

    /** @test */
    public function looseWeightProduct()
    {
        $this->cart->addItemQuantity($this->apples, 0.5);

        Approvals::verifyString($this->printReceipt());
    }

    /** @test */
    public function percentDiscount()
    {
        $this->cart->addItem($this->rice);
        $this->teller->addSpecialOffer(SpecialOfferType::TEN_PERCENT_DISCOUNT(), $this->rice, 10.0);

        Approvals::verifyString($this->printReceipt());
    }

    /** @test */
    public function xForYDiscount()
    {
        $this->cart->addItem($this->cherryTomatoes);
        $this->cart->addItem($this->cherryTomatoes);
        $this->teller->addSpecialOffer(SpecialOfferType::TWO_FOR_AMOUNT(), $this->cherryTomatoes, 0.99);

        Approvals::verifyString($this->printReceipt());
    }

    /** @test */
    public function xForYDiscountWithInsufficientInBasket()
    {
        $this->cart->addItem($this->cherryTomatoes);
        $this->teller->addSpecialOffer(SpecialOfferType::TWO_FOR_AMOUNT(), $this->cherryTomatoes, 0.99);

        Approvals::verifyString($this->printReceipt());
    }

    /** @test */
    public function fiveForYDiscount()
    {
        $this->cart->addItemQuantity($this->apples, 5);
        $this->teller->addSpecialOffer(SpecialOfferType::FIVE_FOR_AMOUNT(), $this->apples, 6.99);

        Approvals::verifyString($this->printReceipt());
    }

    /** @test */
    public function fiveForYDiscountWithSix()
    {
        $this->cart->addItemQuantity($this->apples, 6);
        $this->teller->addSpecialOffer(SpecialOfferType::FIVE_FOR_AMOUNT(), $this->apples, 6.99);

        Approvals::verifyString($this->printReceipt());
    }

    /** @test */
    public function fiveForYDiscountWithSixteen()
    {
        $this->cart->addItemQuantity($this->apples, 16);
        $this->teller->addSpecialOffer(SpecialOfferType::FIVE_FOR_AMOUNT(), $this->apples, 6.99);

        Approvals::verifyString($this->printReceipt());
    }

    /** @test */
    public function fiveForYDiscountWithFour()
    {
        $this->cart->addItemQuantity($this->apples, 4);
        $this->teller->addSpecialOffer(SpecialOfferType::FIVE_FOR_AMOUNT(), $this->apples, 6.99);

        Approvals::verifyString($this->printReceipt());
    }

    /**
     * @return string
     */
    private function printReceipt(): string
    {
        $receipt = $this->teller->checkoutArticlesFrom($this->cart);
        return $this->printer->printReceipt($receipt);
    }
}
