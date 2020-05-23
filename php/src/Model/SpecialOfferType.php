<?php

namespace Supermarket\Model;

use MyCLabs\Enum\Enum;

/**
 * Class SpecialOfferType
 *
 * @method static SpecialOfferType threeForTwo
 * @method static SpecialOfferType tenPercentDiscount
 * @method static SpecialOfferType twoForAmount
 * @method static SpecialOfferType fiveForAmount
 * @method bool equals(SpecialOfferType $type = null)
 * @package Supermarket
 */
class SpecialOfferType extends Enum
{
    private const threeForTwo = 0;
    private const tenPercentDiscount = 1;
    private const twoForAmount = 2;
    private const fiveForAmount = 3;
}