<?php

/** @noinspection PhpIncompatibleReturnTypeInspection */

/** @noinspection PhpStrictTypeCheckingInspection */

declare(strict_types=1);

namespace Tests;

use SplObjectStorage;
use Supermarket\model\Product;
use Supermarket\model\SupermarketCatalog;

class FakeCatalog implements SupermarketCatalog
{
    /**
     * @var splObjectStorage
     */
    private $products;

    /**
     * @var splObjectStorage
     */
    private $prices;

    public function __construct()
    {
        $this->products = new splObjectStorage();
        $this->prices = new splObjectStorage();
    }

    public function addProduct(Product $product, float $price): void
    {
        $this->products->attach($product);
        $this->prices->attach($product, $price);
    }

    public function getUnitPrice(Product $product): float
    {
        return $this->prices[$product];
    }
}
