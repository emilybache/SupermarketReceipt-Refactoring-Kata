from dataclasses import dataclass
from typing import List

from supermarket.product import Product
from supermarket.offer import Discount


@dataclass(frozen=True)
class ReceiptItem:
    product: Product
    quantity: float
    price: float
    total_price: float


class Receipt:
    def __init__(self) -> None:
        self._items: List[ReceiptItem] = []
        self._discounts: List[Discount] = []

    def total_price(self) -> float:
        return sum(i.total_price for i in self.items) + sum(
            d.discount_amount for d in self.discounts
        )

    def add_product(
        self, product: Product, quantity: float, price: float, total_price: float
    ) -> None:
        # TODO: pass the object instead of so many arguments
        self._items.append(ReceiptItem(product, quantity, price, total_price))

    def add_discount(self, discount: Discount) -> None:
        self._discounts.append(discount)

    @property
    def items(self) -> List[ReceiptItem]:
        return self._items[:]

    @property
    def discounts(self) -> List[Discount]:
        return self._discounts[:]
