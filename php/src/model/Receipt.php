<?php

declare(strict_types=1);

namespace Supermarket\model;

class Receipt
{
    /**
     * @var array
     */
    private $items = [];

    /**
     * @var array
     */
    private $discounts = [];

    public function getTotalPrice(): float
    {
        $total = 0.0;

        /** @var ReceiptItem $item */
        foreach ($this->items as $item) {
            $total += $item->getTotalPrice();
        }
        /** @var Discount $discount */
        foreach ($this->discounts as $discount) {
            $total += $discount->getDiscountAmount();
        }
        return $total;
    }

    public function addProduct(Product $product, float $quantity, float $price, float $totalPrice): void
    {
        $this->items[] = new ReceiptItem($product, $quantity, $price, $totalPrice);
    }

    public function getItems(): array
    {
        return $this->items;
    }

    public function addDiscount(Discount $discount): void
    {
        $this->discounts[] = $discount;
    }

    public function getDiscounts(): array
    {
        return $this->discounts;
    }
}
