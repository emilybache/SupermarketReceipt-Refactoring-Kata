<?php

declare(strict_types=1);

namespace Supermarket\Model;

interface SupermarketCatalog
{
    public function addProduct(Product $product, float $price): void;

    public function getUnitPrice(Product $product): float;
}
