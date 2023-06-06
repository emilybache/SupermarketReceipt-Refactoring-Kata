from supermarket.product import ProductName, ProductQuantity, Product

from collections import defaultdict
from typing import List, Dict


class ShoppingCart:
    def __init__(self) -> None:
        self._items: List[ProductQuantity] = []
        self._product_quantities: Dict[ProductName, float] = defaultdict(float)

    @property
    def items(self) -> List[ProductQuantity]:
        return self._items

    def add_item(self, product: Product) -> None:
        self.add_item_quantity(product, 1.0)

    @property
    def product_quantities(self) -> Dict[ProductName, float]:
        return self._product_quantities

    def add_item_quantity(self, product: Product, quantity: float) -> None:
        self._items.append(ProductQuantity(product, quantity))
        self._product_quantities[product.name] += quantity
