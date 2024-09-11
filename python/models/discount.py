from .product import Product

class Discount:
    def __init__(self, product: Product, description: str, discount_amount: int):
        self.product = product
        self.description = description
        self.discount_amount = discount_amount

    def __repr__(self):
        return (f"Discount(product={self.product}, "
                f"description={self.description}, discount_amount={self.discount_amount})")