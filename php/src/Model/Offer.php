<?php

declare(strict_types=1);

namespace Supermarket\Model;

class Offer
{
    /**
     * @var Product
     */
    private $product;

    /**
     * @var SpecialOfferType
     */
    private $offerType;

    /**
     * @var float
     */
    private $argument;

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
