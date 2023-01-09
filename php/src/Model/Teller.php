<?php

declare(strict_types=1);

namespace Supermarket\Model;

use Ds\Map;

class Teller
{
    /**
     * @var Map<Product, Offer>
     */
    private Map $offers;

    public function __construct(
        private SupermarketCatalog $catalog
    ) {
        $this->offers = new Map();
    }

    public function addSpecialOffer(SpecialOfferType $offerType, Product $product, float $argument): void
    {
        $this->offers[$product] = new Offer($offerType, $product, $argument);
    }

    public function checkoutArticlesFrom(ShoppingCart $cart): Receipt
    {
        $receipt = new Receipt();
        $productQuantities = $cart->getItems();
        foreach ($productQuantities as $pq) {
            $p = $pq->getProduct();
            $quantity = $pq->getQuantity();
            $unitPrice = $this->catalog->getUnitPrice($p);
            $price = $quantity * $unitPrice;
            $receipt->addProduct($p, $quantity, $unitPrice, $price);
        }

        $cart->handleOffers($receipt, $this->offers, $this->catalog);

        return $receipt;
    }
}
