from catalog import SupermarketCatalog

from models.product import Product

class FakeCatalog(SupermarketCatalog):
    def __init__(self):
        self.products = {}
        self.prices = {}

    def add_product(self, product: Product, price: int):
        self.products[product.name] = product
        self.prices[product.name] = price

    def unit_price(self, product: Product):
        return self.prices[product.name]

