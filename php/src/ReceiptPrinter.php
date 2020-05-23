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
            $itemPresentation = $this->presentReceiptItem($item);
            $result .= $itemPresentation;
        }

        foreach ($receipt->getDiscounts() as $discount) {
            $discountPresentation = $this->presentDiscount($discount, $result);
            $result .= $discountPresentation;
        }

        $result .= "\n";
        $result .= $this->presentTotal($receipt);
        return $result;
    }

    /**
     * @param ReceiptItem $item
     * @return string
     */
    protected function presentReceiptItem(ReceiptItem $item): string
    {
        $price = self::presentPrice($item->getTotalPrice());
        $name = $item->getProduct()->getName();

        $line = $this->formatLineWithWhitespace($name, $price);

        if ($item->getQuantity() != 1) {
            $line .= ' ' . self::presentPrice($item->getPrice()) . ' * ' . self::presentQuantity($item) . "\n";
        }
        return $line;
    }

    /**
     * @param Model\Discount $discount
     * @param string         $result
     * @return string
     */
    protected function presentDiscount(Model\Discount $discount, string $result): string
    {
        $name = "{$discount->getDescription()}({$discount->getProduct()->getName()})";
        $value = self::presentPrice($discount->getDiscountAmount());

        return $this->formatLineWithWhitespace($name, $value);
    }

    /**
     * @param Receipt $receipt
     * @return string
     */
    protected function presentTotal(Receipt $receipt): string
    {
        $name = "Total :";
        $value = self::presentPrice($receipt->getTotalPrice());
        return $this->formatLineWithWhitespace($name, $value);
    }

    /**
     * @param string $name
     * @param string $value
     * @return string
     */
    protected function formatLineWithWhitespace(string $name, string $value): string
    {
        $whitespaceSize = $this->columns - strlen($name) - strlen($value);
        return $name . str_repeat(' ', $whitespaceSize) . $value . "\n";
    }

    /**
     * @param float $price
     * @return string
     */
    protected static function presentPrice(float $price): string
    {
        return sprintf('%.2F', $price);
    }

    private static function presentQuantity(ReceiptItem $item): string
    {
        return $item->getProduct()->getUnit()->equals(ProductUnit::each()) ?
            sprintf('%x', $item->getQuantity()) :
            sprintf('%.3F', $item->getQuantity());
    }
}