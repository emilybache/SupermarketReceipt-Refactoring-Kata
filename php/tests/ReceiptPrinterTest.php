<?php

declare(strict_types=1);

namespace Tests;

use ApprovalTests\Approvals;
use PHPUnit\Framework\TestCase;
use Supermarket\Model\Discount;
use Supermarket\Model\Product;
use Supermarket\Model\ProductUnit;
use Supermarket\Model\Receipt;
use Supermarket\ReceiptPrinter;

class ReceiptPrinterTest extends TestCase
{
    /**
     * @var Product
     */
    private $toothbrush;

    /**
     * @var Product
     */
    private $apples;

    /**
     * @var Receipt
     */
    private $receipt;

    /**
     * @var ReceiptPrinter
     */
    private $printer;

    protected function setUp(): void
    {
        parent::setUp();
        $this->toothbrush = new Product('toothbrush', ProductUnit::EACH());
        $this->apples = new Product('apples', ProductUnit::KILO());
        $this->receipt = new Receipt();
        $this->printer = new ReceiptPrinter(40);
    }

    public function testOneLineItem(): void
    {
        $this->receipt->addProduct($this->toothbrush, 1, 0.99, 0.99);
        Approvals::verifyString($this->printReceipt());
    }

    public function testQuantityTwo(): void
    {
        $this->receipt->addProduct($this->toothbrush, 2, 0.99, 2 * 0.99);
        Approvals::verifyString($this->printReceipt());
    }

    public function testLooseWeight(): void
    {
        $this->receipt->addProduct($this->apples, 2.3, 1.99, 2.3 * 1.99);
        Approvals::verifyString($this->printReceipt());
    }

    public function testTotal(): void
    {
        $this->receipt->addProduct($this->toothbrush, 1, 0.99, 2 * 0.99);
        $this->receipt->addProduct($this->apples, 0.75, 1.99, 0.75 * 1.99);
        Approvals::verifyString($this->printReceipt());
    }

    public function testDiscounts(): void
    {
        $this->receipt->addDiscount(new Discount($this->apples, '3 for 2', -0.99));
        Approvals::verifyString($this->printReceipt());
    }

    public function testPrintWholeReceipt(): void
    {
        $this->receipt->addProduct($this->toothbrush, 1, 0.99, 0.99);
        $this->receipt->addProduct($this->toothbrush, 2, 0.99, 2 * 0.99);
        $this->receipt->addProduct($this->apples, 0.75, 1.99, 1.99 * 0.75);
        $this->receipt->addDiscount(new Discount($this->toothbrush, '3 for 2', -0.99));
        Approvals::verifyString($this->printReceipt());
    }

    private function printReceipt(): string
    {
        return $this->printer->printReceipt($this->receipt);
    }
}
