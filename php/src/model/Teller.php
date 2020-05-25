<?php

declare(strict_types=1);

namespace Supermarket\model;

use SplObjectStorage;

class Teller
{
    /**
     * @var SupermarketCatalog
     */
    private $catalog;

    /**
     * @var SplObjectStorage
     */
    private $offers;

    public function __construct(SupermarketCatalog $catalog)
    {
        $this->catalog = $catalog;
        $this->offers = new SplObjectStorage();
    }

    public function addSpecialOffer(string $offerType, Product $product, float $argument): void
    {
        $this->offers[$product] = new Offer($offerType, $product, $argument);
    }

    public function checksOutArticlesFrom(ShoppingCart $theCart): Receipt
    {
        $receipt = new Receipt();
        $productQuantities = $theCart->getItems();

        /** @var ProductQuantity $productQuantity */
        foreach ($productQuantities as $productQuantity) {
            $product = $productQuantity->getProduct();
            $quantity = $productQuantity->getQuantity();
            $unitPrice = $this->catalog->getUnitPrice($product);
            $price = $quantity * $unitPrice;
            $receipt->addProduct($product, $quantity, $unitPrice, $price);
        }
        $theCart->handleOffers($receipt, $this->offers, $this->catalog);

        return $receipt;
    }
}
