<?php

declare(strict_types=1);

namespace Supermarket\Model;

class Discount
{
    public function __construct(
        private Product $product,
        private string $description,
        private float $discount
    ) {
    }

    public function getDescription(): string
    {
        return $this->description;
    }

    public function getDiscountAmount(): float
    {
        return $this->discount;
    }

    public function getProduct(): Product
    {
        return $this->product;
    }
}
