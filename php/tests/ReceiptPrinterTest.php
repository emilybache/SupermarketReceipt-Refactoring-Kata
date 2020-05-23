<?php

namespace Tests;

use ApprovalTests\Approvals;
use Supermarket\Model\Product;
use Supermarket\Model\Receipt;
use PHPUnit\Framework\TestCase;
use Supermarket\ReceiptPrinter;
use Supermarket\Model\Discount;
use Supermarket\Model\ProductUnit;

class ReceiptPrinterTest extends TestCase
{
    private Product $toothbrush;
    private Product $apples;
    private Receipt $receipt;
    private ReceiptPrinter $printer;

    public function setUp(): void
    {
        parent::setUp();
        $this->toothbrush = new Product('toothbrush', ProductUnit::each());
        $this->apples = new Product('apples', ProductUnit::kilo());
        $this->receipt = new Receipt();
        $this->printer = new ReceiptPrinter(40);
    }

    /** @test */
    public function oneLineItem()
    {
        $this->receipt->addProduct($this->toothbrush, 1, 0.99, 0.99);
        Approvals::verifyString($this->printReceipt());
    }

    /** @test */
    public function quantityTwo()
    {
        $this->receipt->addProduct($this->toothbrush, 2, 0.99, 2 * 0.99);
        Approvals::verifyString($this->printReceipt());
    }

    /** @test */
    public function looseWeight()
    {
        $this->receipt->addProduct($this->apples, 2.3, 1.99, 2.3 * 1.99);
        Approvals::verifyString($this->printReceipt());
    }

    /** @test */
    public function total()
    {
        $this->receipt->addProduct($this->toothbrush, 1, 0.99, 2 * 0.99);
        $this->receipt->addProduct($this->apples, 0.75, 1.99, 0.75 * 1.99);
        Approvals::verifyString($this->printReceipt());
    }

    /** @test */
    public function discounts()
    {
        $this->receipt->addDiscount(new Discount($this->apples, '3 for 2', -0.99));
        Approvals::verifyString($this->printReceipt());
    }

    /** @test */
    public function printWholeReceipt()
    {
        $this->receipt->addProduct($this->toothbrush, 1, 0.99, 0.99);
        $this->receipt->addProduct($this->toothbrush, 2, 0.99, 2 * 0.99);
        $this->receipt->addProduct($this->apples, 0.75, 1.99, 1.99 * 0.75);
        $this->receipt->addDiscount(new Discount($this->toothbrush, "3 for 2", -0.99));
        Approvals::verifyString($this->printReceipt());
    }

    /**
     * @return string
     */
    private function printReceipt(): string
    {
        return $this->printer->printReceipt($this->receipt);
    }
}
