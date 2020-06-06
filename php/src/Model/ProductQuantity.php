<?php

namespace Supermarket\Model;

class ProductQuantity
{
    private $product;
    private $quantity;

    public function __construct(Product $product, float $weight)
    {
        $this->product = $product;
        $this->quantity = $weight;
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
