from typing import Dict

from supermarket.product import Product, ProductName


class SupermarketCatalog:
    def __init__(self) -> None:
        self.products: Dict[ProductName, Product] = {}
        self.prices: Dict[ProductName, float] = {}

    def add_product(self, product: Product, price: float) -> None:
        raise Exception("cannot be called from a unit test - it accesses the database")

    def unit_price(self, product_name: ProductName) -> float:
        raise Exception("cannot be called from a unit test - it accesses the database")
