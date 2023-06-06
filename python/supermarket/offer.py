import math
from abc import ABCMeta, abstractmethod
from enum import Enum
from dataclasses import dataclass
from typing import Any, Optional, Sequence

from supermarket.product import Product


@dataclass(frozen=True)
class Discount:
    product: Product
    description: str
    discount_amount: float


class SpecialOfferType(Enum):
    PERCENTAGE_DISCOUNT = 1
    QUANTITY_FOR_AMOUNT = 2
    QUANTITY_FOR_QUANTITY = 3


class Offer(metaclass=ABCMeta):
    def __init__(self, product: Product, argument: Optional[Any] = None) -> None:
        self.product = product
        self.argument = argument

    def calculate_discount(self, unit_price: float, purchased_quantity: float):
        raise NotImplementedError


class QuantityForQuantity(Offer):

    TYPE = SpecialOfferType.QUANTITY_FOR_QUANTITY

    def calculate_discount(self, unit_price: float, purchased_quantity: float):
        assert isinstance(self.argument, Sequence)
        quantity_x, quantity_y = self.argument
        purchased_quantity_as_int = int(purchased_quantity)
        number_of_x = math.floor(purchased_quantity_as_int / quantity_x)
        if purchased_quantity_as_int > quantity_y:
            discount_amount = purchased_quantity * unit_price - (
                (number_of_x * quantity_y * unit_price) + purchased_quantity_as_int % quantity_x * unit_price
            )
            return Discount(
                self.product,
                f"{quantity_x} for {quantity_y}",
                -discount_amount
            )


class QuantityForAmount(Offer):

    TYPE = SpecialOfferType.QUANTITY_FOR_AMOUNT

    def calculate_discount(self, unit_price: float, purchased_quantity: float):
        assert isinstance(self.argument, Sequence)
        offer_quantity, offer_amount = self.argument
        if purchased_quantity < offer_quantity:
            return

        purchased_quantity_as_int = int(purchased_quantity)
        effective_offers = math.floor(purchased_quantity_as_int / offer_quantity)
        remaining_quantity = purchased_quantity_as_int % offer_quantity
        total_amount = (
            offer_amount * effective_offers + remaining_quantity * unit_price
        )
        discount_amount = unit_price * purchased_quantity - total_amount
        discount_text = f"{offer_quantity} for {offer_amount}"
        return Discount(self.product, discount_text, -discount_amount)


class PercentageDiscount(Offer):

    TYPE = SpecialOfferType.PERCENTAGE_DISCOUNT

    def calculate_discount(self, unit_price: float, purchased_quantity: float):
        discount_percent = self.argument
        assert isinstance(discount_percent, float)
        return Discount(
            self.product,
            f"{self.argument}% off",
            -purchased_quantity * unit_price * discount_percent / 100.0,
        )


SPECIAL_OFFER_IMPLEMENTATIONS = {
    SpecialOfferType.QUANTITY_FOR_QUANTITY: QuantityForQuantity,
    SpecialOfferType.PERCENTAGE_DISCOUNT: PercentageDiscount,
    SpecialOfferType.QUANTITY_FOR_AMOUNT: QuantityForAmount,
}


def create_offer(
    offer_type: SpecialOfferType, product: Product, argument: Optional[Any] = None
):
    try:
        return SPECIAL_OFFER_IMPLEMENTATIONS[offer_type](product, argument)
    except KeyError:
        raise NotImplementedError(f"Unknown offer type: {offer_type}")
