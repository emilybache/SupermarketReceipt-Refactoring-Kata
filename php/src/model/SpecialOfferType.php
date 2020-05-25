<?php

declare(strict_types=1);

namespace Supermarket\model;

abstract class SpecialOfferType
{
    public const THREE_FOR_TWO = 'ThreeForTwo';

    public const TEN_PERCENT_DISCOUNT = 'TenPercentDiscount';

    public const TWO_FOR_AMOUNT = 'TwoForAmount';

    public const FIVE_FOR_AMOUNT = 'FiveForAmount';
}
