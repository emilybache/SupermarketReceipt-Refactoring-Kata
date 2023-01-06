<?php

declare(strict_types=1);

namespace Supermarket\Model;

class ProductQuantity
{
    public function __construct(
        private Product $product,
        private float $quantity
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
}
