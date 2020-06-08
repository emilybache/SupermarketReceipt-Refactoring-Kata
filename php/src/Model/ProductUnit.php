<?php

declare(strict_types=1);

namespace Supermarket\Model;

use MyCLabs\Enum\Enum;

/**
 * Class ProductUnit
 *
 * @method static ProductUnit EACH
 * @method static ProductUnit KILO
 * @method equals(ProductUnit $unit)
 * @package Supermarket
 */
class ProductUnit extends Enum
{
    private const EACH = 'each';

    private const KILO = 'kilo';
}
