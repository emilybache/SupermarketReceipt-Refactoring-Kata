<?php

declare(strict_types=1);

namespace Supermarket\Model;

class ReceiptItem
{
    public function __construct(
        private Product $product,
        private float $quantity,
        private float $price,
        private float $totalPrice
    ) {
    }

    public function getProduct(): Product
    {
        return $this->product;
    }

    public function getQuantity(): float
    {
        return $this->quantity;
    }

    public function getPrice(): float
    {
        return $this->price;
    }

    public function getTotalPrice(): float
    {
        return $this->totalPrice;
    }
}
