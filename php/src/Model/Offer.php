<?php

declare(strict_types=1);

namespace Supermarket\Model;

class Offer
{
    public function __construct(
        private SpecialOfferType $offerType,
        private Product $product,
        private float $argument
    ) {
    }

    public function getArgument(): float
    {
        return $this->argument;
    }

    public function getProduct(): Product
    {
        return $this->product;
    }

    public function getOfferType(): SpecialOfferType
    {
        return $this->offerType;
    }
}
