from dataclasses import dataclass
from enum import Enum


ProductName = str


class ProductUnit(Enum):
    EACH = 1
    KILO = 2


@dataclass(frozen=True)
class Product:
    name: ProductName
    unit: ProductUnit


@dataclass(frozen=True)
class ProductQuantity:
    product: Product
    quantity: float
