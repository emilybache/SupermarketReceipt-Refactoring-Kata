<?php

declare(strict_types=1);

namespace Supermarket\Model;

use Ds\Map;

class ShoppingCart
{
    /**
     * @var ProductQuantity[]
     */
    private array $items = [];

    /**
     * @var Map<Product, float>
     */
    private Map $productQuantities;

    public function __construct()
    {
        $this->productQuantities = new Map();
    }

    public function addItem(Product $product): void
    {
        $this->addItemQuantity($product, 1.0);
    }

    /**
     * @return ProductQuantity[]
     */
    public function getItems(): array
    {
        return $this->items;
    }

    public function addItemQuantity(Product $product, float $quantity): void
    {
        $this->items[] = new ProductQuantity($product, $quantity);
        if ($this->productQuantities->hasKey($product)) {
            $newAmount = $this->productQuantities[$product] + $quantity;
            $this->productQuantities[$product] = $newAmount;
        } else {
            $this->productQuantities[$product] = $quantity;
        }
    }

    /**
     * @param Map<Product, Offer> $offers
     */
    public function handleOffers(Receipt $receipt, Map $offers, SupermarketCatalog $catalog): void
    {
        /**
         * @var Product $p
         * @var float $quantity
         */
        foreach ($this->productQuantities as $p => $quantity) {
            $quantityAsInt = (int) $quantity;
            if ($offers->hasKey($p)) {
                /** @var Offer $offer */
                $offer = $offers[$p];
                $unitPrice = $catalog->getUnitPrice($p);
                $discount = null;
                $x = 1;
                if ($offer->getOfferType()->equals(SpecialOfferType::THREE_FOR_TWO())) {
                    $x = 3;
                } elseif ($offer->getOfferType()->equals(SpecialOfferType::TWO_FOR_AMOUNT())) {
                    $x = 2;
                    if ($quantityAsInt >= 2) {
                        $total = $offer->getArgument() * intdiv($quantityAsInt, $x) + $quantityAsInt % 2 * $unitPrice;
                        $discountN = $unitPrice * $quantity - $total;
                        $discount = new Discount($p, "2 for {$offer->getArgument()}", -1 * $discountN);
                    }
                }

                if ($offer->getOfferType()->equals(SpecialOfferType::FIVE_FOR_AMOUNT())) {
                    $x = 5;
                }
                $numberOfXs = intdiv($quantityAsInt, $x);
                if ($offer->getOfferType()->equals(SpecialOfferType::THREE_FOR_TWO()) && $quantityAsInt > 2) {
                    $discountAmount = $quantity * $unitPrice - ($numberOfXs * 2 * $unitPrice + $quantityAsInt % 3 * $unitPrice);
                    $discount = new Discount($p, '3 for 2', -$discountAmount);
                }

                if ($offer->getOfferType()->equals(SpecialOfferType::TEN_PERCENT_DISCOUNT())) {
                    $discount = new Discount(
                        $p,
                        "{$offer->getArgument()}% off",
                        -$quantity * $unitPrice * $offer->getArgument() / 100.0
                    );
                }
                if ($offer->getOfferType()->equals(SpecialOfferType::FIVE_FOR_AMOUNT()) && $quantityAsInt >= 5) {
                    $discountTotal = $unitPrice * $quantity - ($offer->getArgument() * $numberOfXs + $quantityAsInt % 5 * $unitPrice);
                    $discount = new Discount($p, "${x} for {$offer->getArgument()}", -$discountTotal);
                }

                if ($discount !== null) {
                    $receipt->addDiscount($discount);
                }
            }
        }
    }
}
