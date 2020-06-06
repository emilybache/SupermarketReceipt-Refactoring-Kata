<?php

namespace Supermarket\Model;

class ReceiptItem
{
    private $product;
    private $quantity;
    private $price;
    private $totalPrice;

    public function __construct(Product $product, float $quantity, float $price, float $totalPrice)
    {
        $this->product = $product;
        $this->quantity = $quantity;
        $this->price = $price;
        $this->totalPrice = $totalPrice;
    }

    public function getProduct(): Product
    {
        return $this->product;
    }

    public function getQuantity(): float
    {
        return $this->quantity;
    }

    public function getPrice(): float
    {
        return $this->price;
    }

    public function getTotalPrice(): float
    {
        return $this->totalPrice;
    }
}
