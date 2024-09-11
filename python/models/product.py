from enum import Enum
from dataclasses import dataclass

@dataclass(frozen=True)
class ProductUnit(Enum):
    EACH = 1
    KILO = 2

class Product:
    def __init__(self, name: str, unit: ProductUnit):
        self.name = name
        self.unit = unit

    def __repr__(self):
        return f"Product(name={self.name}, unit={self.unit.name})"

class ProductQuantity:
    def __init__(self, product: Product, quantity: int):
        self.product = product
        self.quantity = quantity

    def __repr__(self):
        return f"ProductQuantity(product={self.product}, quantity={self.quantity})"