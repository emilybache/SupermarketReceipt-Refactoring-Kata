<?php

declare(strict_types=1);

namespace Supermarket\model;

use SplObjectStorage;

class ShoppingCart
{
    /**
     * @var array
     */
    private $items = [];

    /**
     * @var SplObjectStorage
     */
    private $productQuantities;

    public function __construct()
    {
        $this->productQuantities = new SplObjectStorage();
    }

    public function getItems(): array
    {
        return $this->items;
    }

    public function addItem(Product $product): void
    {
        $this->addItemQuantity($product, 1.0);
    }

    public function productQuantities(): SplObjectStorage
    {
        return $this->productQuantities;
    }

    public function addItemQuantity(Product $product, float $quantity): void
    {
        $this->items[] = new ProductQuantity($product, $quantity);
        if (isset($this->productQuantities[$product])) {
            $this->productQuantities[$product] += $quantity;
        } else {
            $this->productQuantities[$product] = $quantity;
        }
    }

    public function handleOffers(Receipt $receipt, SplObjectStorage $offers, SupermarketCatalog $catalog): void
    {
        $this->productQuantities->rewind();
        /** @var Product $product */
        foreach ($this->productQuantities as $product) {
            $quantity = $this->productQuantities->getInfo();
            if ($offers->contains($product)) {
                /** @var Offer $offer */
                $offer = $offers[$product];
                $unitPrice = $catalog->getUnitPrice($product);
                $quantityAsInt = (int) $quantity;
                /** @var Discount $discount */
                $discount = null;
                $x = 1;
                if ($offer->offerType === SpecialOfferType::THREE_FOR_TWO) {
                    $x = 3;
                } elseif ($offer->offerType === SpecialOfferType::TWO_FOR_AMOUNT) {
                    $x = 2;
                    if ($quantityAsInt >= 2) {
                        /** @var double $total */
                        $total = $offer->argument * ($quantityAsInt / $x) + $quantityAsInt % 2 * $unitPrice;
                        /** @var double $discountN */
                        $discountN = $unitPrice * $quantity - $total;
                        $discount = new Discount($product, '2 for ' . $offer->argument, -$discountN);
                    }
                }
                if ($offer->offerType === SpecialOfferType::FIVE_FOR_AMOUNT) {
                    $x = 5;
                }
                /** @var int $numberOfXs */
                $numberOfXs = floor($quantityAsInt / $x);
                if ($offer->offerType === SpecialOfferType::THREE_FOR_TWO && $quantityAsInt > 2) {
                    /** @var double $discountAmount */
                    $discountAmount = $quantity * $unitPrice
                        - (($numberOfXs * 2 * $unitPrice) + ($quantityAsInt % 3) * $unitPrice);
                    $discount = new Discount($product, '3 for 2', -$discountAmount);
                }
                if ($offer->offerType === SpecialOfferType::TEN_PERCENT_DISCOUNT) {
                    $discount = new Discount(
                        $product,
                        sprintf('%.1f', $offer->argument) . '% off',
                        -$quantity * $unitPrice * $offer->argument / 100.0
                    );
                }
                if ($offer->offerType === SpecialOfferType::FIVE_FOR_AMOUNT && $quantityAsInt >= 5) {
                    $discountTotal = $unitPrice * $quantity
                        - ($offer->argument * $numberOfXs + ($quantityAsInt % 5) * $unitPrice);
                    $discount = new Discount($product, $x . ' for ' . $offer->argument, -$discountTotal);
                }
                if ($discount !== null) {
                    $receipt->addDiscount($discount);
                }
            }
        }
    }
}
