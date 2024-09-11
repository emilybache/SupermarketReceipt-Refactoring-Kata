from typing import List

from models.product import Product
from models.discount import Discount

""" all amounts will be int in cents instead of float"""

class ReceiptItem:
    def __init__(self, product: Product, quantity: int, price: int, total_price: int):
        self.product = product
        self.quantity = quantity
        self.price = price
        self.total_price = total_price

    def __repr__(self):
        return (f"ReceiptItem(product={self.product}, quantity={self.quantity}, "
                f"price={self.price}, total_price={self.total_price})")

class Receipt:
    def __init__(self):
        self._items: List[ReceiptItem] = []
        self._discounts: List[Discount] = []

    @property
    def items(self) -> List[ReceiptItem]:
        return self._items[:]

    @property
    def discounts(self) -> List[Discount]:
        return self._discounts[:]

    def total_price(self) -> int:
        total = 0
        total = sum(item.total_price for item in self._items)
        total += sum(discount.discount_amount for discount in self._discounts)
        return total

    def add_product(self, product: Product, quantity: int, price: int, total_price: int):
        if quantity < 0 or price < 0 or total_price < 0:
            raise ValueError("Quantity, price, and total price must be non-negative.")
        self._items.append(ReceiptItem(product, quantity, price, total_price))

    def add_discount(self, discount: Discount):
        if discount.discount_amount > 0:
            raise ValueError("Discount amount must be negative.")
        self._discounts.append(discount)

