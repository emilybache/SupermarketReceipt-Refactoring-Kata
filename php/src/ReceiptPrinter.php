<?php

namespace Supermarket;

use Supermarket\Model\{
    Receipt,
    ReceiptItem,
    ProductUnit
};

class ReceiptPrinter
{
    private int $columns;

    public function __construct(int $columns = 40)
    {
        $this->columns = $columns;
    }

    public function printReceipt(Receipt $receipt)
    {
        $result = "";
        foreach ($receipt->getItems() as $item) {
            $price = sprintf('%.2F', $item->getPrice());
            $name = $item->getProduct()->getName();

            $whitespaceSize = $this->columns - strlen($name) - strlen($price);
            $line = $name . self::whitespace($whitespaceSize) . $price . "\n";

            if ($item->getQuantity() != 1) {
                $quantity = self::presentQuantity($item);
                $unitPrice = sprintf('%.2F', $item->getPrice());
                $line .= ' ' . $unitPrice . ' * ' . $quantity . "\n";
            }

            $result .= $line;
        }

        foreach ($receipt->getDiscounts() as $discount) {
            $productPresentation = $discount->getProduct()->getName();
            $pricePresentation = sprintf('%.2F', $discount->getDiscountAmount());
            $description = $discount->getDescription();
            $result .= "$description($productPresentation)";
            $result .= self::whitespace($this->columns - 2 - strlen($productPresentation) - strlen($description) - strlen($pricePresentation));
            $result .= $pricePresentation;
            $result .= "\n";
        }

        $result .= "\n";
        $pricePresentation = sprintf('%.2F', $receipt->getTotalPrice());
        $total = "Total :";
        $whitespace = self::whitespace($this->columns - strlen($total) - strlen($pricePresentation));
        $result .= $total . $whitespace . $pricePresentation;
        return $result;
    }

    private static function presentQuantity(ReceiptItem $item): string
    {
        return $item->getProduct()->getUnit()->equals(ProductUnit::each()) ?
            sprintf('%x', $item->getQuantity()) :
            sprintf('%.3F', $item->getQuantity());
    }

    private static function whitespace(int $whitespaceSize)
    {
        return str_repeat(' ', $whitespaceSize);
    }
}