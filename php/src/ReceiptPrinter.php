<?php

declare(strict_types=1);

namespace Supermarket;

use Supermarket\Model\Discount;
use Supermarket\Model\ProductUnit;
use Supermarket\Model\Receipt;
use Supermarket\Model\ReceiptItem;

class ReceiptPrinter
{
    public function __construct(
        private int $columns = 40
    ) {
    }

    public function printReceipt(Receipt $receipt): string
    {
        $result = '';
        foreach ($receipt->getItems() as $item) {
            $itemPresentation = $this->presentReceiptItem($item);
            $result .= $itemPresentation;
        }

        foreach ($receipt->getDiscounts() as $discount) {
            $discountPresentation = $this->presentDiscount($discount);
            $result .= $discountPresentation;
        }

        $result .= "\n";
        $result .= $this->presentTotal($receipt);
        return $result;
    }

    protected function presentReceiptItem(ReceiptItem $item): string
    {
        $price = self::presentPrice($item->getTotalPrice());
        $name = $item->getProduct()->getName();

        $line = $this->formatLineWithWhitespace($name, $price) . "\n";

        if ($item->getQuantity() !== 1.0) {
            $line .= '  ' . self::presentPrice($item->getPrice()) . ' * ' . self::presentQuantity($item) . "\n";
        }
        return $line;
    }

    protected function presentDiscount(Discount $discount): string
    {
        $name = "{$discount->getDescription()}({$discount->getProduct()->getName()})";
        $value = self::presentPrice($discount->getDiscountAmount());

        return $this->formatLineWithWhitespace($name, $value) . "\n";
    }

    protected function presentTotal(Receipt $receipt): string
    {
        $name = 'Total: ';
        $value = self::presentPrice($receipt->getTotalPrice());
        return $this->formatLineWithWhitespace($name, $value);
    }

    protected function formatLineWithWhitespace(string $name, string $value): string
    {
        $whitespaceSize = $this->columns - strlen($name) - strlen($value);
        return $name . str_repeat(' ', $whitespaceSize) . $value;
    }

    protected static function presentPrice(float $price): string
    {
        return sprintf('%.2F', $price);
    }

    private static function presentQuantity(ReceiptItem $item): string
    {
        return $item->getProduct()->getUnit()->equals(ProductUnit::EACH()) ?
            sprintf('%x', $item->getQuantity()) :
            sprintf('%.3F', $item->getQuantity());
    }
}
