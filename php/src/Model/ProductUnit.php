<?php

namespace Supermarket\Model;

use MyCLabs\Enum\Enum;

/**
 * Class ProductUnit
 *
 * @method static ProductUnit each
 * @method static ProductUnit kilo
 * @method equals(ProductUnit $unit)
 * @package Supermarket
 */
class ProductUnit extends Enum
{
    private const each = 'each';
    private const kilo = 'kilo';
}