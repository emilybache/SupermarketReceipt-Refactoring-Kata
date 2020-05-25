<?php

declare(strict_types=1);

namespace Supermarket\model;

class Offer
{
    /**
     * @var string
     */
    public $offerType;

    /**
     * @var Product
     */
    public $product;

    /**
     * @var float
     */
    public $argument;

    public function __construct(string $offerType, Product $product, float $argument)
    {
        $this->offerType = $offerType;
        $this->argument = $argument;
        $this->product = $product;
    }

    public function getProduct(): Product
    {
        return $this->product;
    }
}
