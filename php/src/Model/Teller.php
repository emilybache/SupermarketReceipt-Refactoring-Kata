<?php

namespace Supermarket\Model;

use Ds\Map;

class Teller
{
    private SupermarketCatalog $catalog;

    /** @var Map [Product => Offer] */
    private Map $offers;

    public function __construct(SupermarketCatalog $catalog)
    {
        $this->catalog = $catalog;
        $this->offers = new Map();
    }

    public function addSpecialOffer(SpecialOfferType $offerType, Product $product, float $argument)
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
