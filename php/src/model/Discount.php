<?php

declare(strict_types=1);

namespace Supermarket\model;

class Discount
{
    /**
     * @var string
     */
    private $description;

    /**
     * @var float
     */
    private $discountAmount;

    /**
     * @var Product
     */
    private $product;

    public function __construct(Product $product, string $description, float $discountAmount)
    {
        $this->product = $product;
        $this->description = $description;
        $this->discountAmount = $discountAmount;
    }

    public function getDescription(): string
    {
        return $this->description;
    }

    public function getDiscountAmount(): float
    {
        return $this->discountAmount;
    }

    public function getProduct(): Product
    {
        return $this->product;
    }
}
