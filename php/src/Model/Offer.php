<?php

namespace Supermarket\Model;

class Offer
{
    private Product $product;
    private SpecialOfferType $offerType;
    private float $argument;

    public function __construct(SpecialOfferType $offerType, Product $product, float $argument)
    {
        $this->offerType = $offerType;
        $this->product = $product;
        $this->argument = $argument;
    }

    public function getArgument(): float
    {
        return $this->argument;
    }

    public function getOfferType(): SpecialOfferType
    {
        return $this->offerType;
    }
}
