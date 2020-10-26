<?php

declare(strict_types=1);

namespace Supermarket\Model;

use MyCLabs\Enum\Enum;

/**
 * Class SpecialOfferType
 *
 * @method static SpecialOfferType THREE_FOR_TWO
 * @method static SpecialOfferType TEN_PERCENT_DISCOUNT
 * @method static SpecialOfferType TWO_FOR_AMOUNT
 * @method static SpecialOfferType FIVE_FOR_AMOUNT
 * @method bool equals(SpecialOfferType $type = null)
 * @package Supermarket
 */
class SpecialOfferType extends Enum
{
    private const THREE_FOR_TWO = 0;

    private const TEN_PERCENT_DISCOUNT = 1;

    private const TWO_FOR_AMOUNT = 2;

    private const FIVE_FOR_AMOUNT = 3;
}
