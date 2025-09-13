from python.domain.model_objects import Discount
from .base_strategy import DiscountStrategy


class TwoForAmountStrategy(DiscountStrategy):
    """Buy 2 for a specific amount strategy (e.g., 2 for €0.99)"""

    def calculate_discount(self, product, quantity, unit_price, offer):
        quantity_as_int = int(quantity)
        if quantity_as_int < 2:
            return None

        pairs = quantity_as_int // 2
        remaining = quantity_as_int % 2

        discounted_total = pairs * offer.argument + remaining * unit_price
        original_total = quantity * unit_price
        discount_amount = original_total - discounted_total

        if discount_amount <= 0:
            return None

        description = f"2 for {offer.argument:.2f}"
        return Discount(product, description, -discount_amount)

    def get_description(self):
        return "Buy 2 for special price"
