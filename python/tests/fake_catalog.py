from supermarket.catalog import SupermarketCatalog
from supermarket.product import Product, ProductName


class FakeCatalog(SupermarketCatalog):
    def __init__(self):
        self.products = {}
        self.prices = {}

    def add_product(self, product: Product, price: float) -> None:
        self.products[product.name] = product
        self.prices[product.name] = price

    def unit_price(self, product_name: ProductName) -> float:
        return self.prices[product_name]
