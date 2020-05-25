<?php

declare(strict_types=1);

namespace Supermarket;

use Supermarket\model\Discount;
use Supermarket\model\Receipt;
use Supermarket\model\ReceiptItem;

class ReceiptPrinter
{
    /**
     * @var int
     */
    private $columns;

    public function __construct(int $columns = 40)
    {
        $this->columns = $columns;
    }

    public function printReceipt(Receipt $receipt): string
    {
        $result = '';

        /** @var ReceiptItem $item */
        foreach ($receipt->getItems() as $item) {
            $receiptItem = $this->presentReceiptItem($item);
            $result .= $receiptItem;
        }
        /** @var Discount $discount */
        foreach ($receipt->getDiscounts() as $discount) {
            $discountPresentation = $this->presentDiscount($discount);
            $result .= $discountPresentation;
        }

        $result .= "\n";
        $result .= $this->presentTotal($receipt);
        return (string) $result;
    }

    private function presentReceiptItem(ReceiptItem $item): string
    {
        $totalPricePresentation = $this->presentPrice($item->getTotalPrice());

        $name = $item->getProduct()->getName();

        $line = $this->formatLineWithWhitespace($name, $totalPricePresentation);

        if ($item->getQuantity() !== 1.0) {
            $line .= '  ' . $this->presentPrice($item->getPrice()) . ' * ' . $this->presentQuantity($item) . "\n";
        }
        return $line;
    }

    private function presentDiscount(Discount $discount): string
    {
        $name = $discount->getDescription() . '(' . $discount->getProduct()->getName() . ')';
        $value = $this->presentPrice($discount->getDiscountAmount());

        return $this->formatLineWithWhitespace($name, $value);
    }

    private function presentTotal(Receipt $receipt): string
    {
        $name = 'Total: ';
        $value = $this->presentPrice($receipt->getTotalPrice());
        return $this->formatLineWithWhitespace($name, $value);
    }

    private function formatLineWithWhitespace(string $name, string $value): string
    {
        $line = '';
        $line .= $name;
        /** @var int $whitespaceSize */
        $whitespaceSize = $this->columns - strlen($name) - strlen($value);
        /** @var int $i */
        for ($i = 0; $i < $whitespaceSize; $i++) {
            $line .= ' ';
        }
        $line .= $value;
        $line .= "\n";
        return (string) $line;
    }

    private function presentPrice(float $price): string
    {
        return sprintf('%.2f', $price);
    }

    private function presentQuantity(ReceiptItem $item): string
    {
        return $item->getProduct()->getUnit() === 'Each'
            ? (string) sprintf('%x', (int) $item->getQuantity())
            : (string) sprintf('%.3f', $item->getQuantity());
    }
}
