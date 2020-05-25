<?php

declare(strict_types=1);

namespace Supermarket\model;

class ProductQuantity
{
    /**
     * @var Product
     */
    private $product;

    /**
     * @var float
     */
    private $quantity;

    public function __construct(Product $product, float $quantity)
    {
        $this->product = $product;
        $this->quantity = $quantity;
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
