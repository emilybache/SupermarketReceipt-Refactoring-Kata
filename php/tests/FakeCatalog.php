<?php

namespace Tests;

use Supermarket\Model\Product;
use Supermarket\Model\SupermarketCatalog;

class FakeCatalog implements SupermarketCatalog
{
    private $prices = [];
    private $products = [];

    public function addProduct(Product $product, float $price): void
    {
        $this->products[$product->getName()] = $product;
        $this->prices[$product->getName()] = $price;
    }

    public function getUnitPrice(Product $product): float
    {
        return $this->prices[$product->getName()];
    }
}
