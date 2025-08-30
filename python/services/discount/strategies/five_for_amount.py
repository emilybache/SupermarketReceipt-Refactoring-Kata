import math
from python.domain.model_objects import Discount
from .base_strategy import DiscountStrategy


class FiveForAmountStrategy(DiscountStrategy):
    """Buy 5 for a specific amount strategy (e.g., 5 for €7.49)"""

    def calculate_discount(self, product, quantity, unit_price, offer):
        quantity_as_int = int(quantity)
        if quantity_as_int < 5:
            return None

        number_of_sets = math.floor(quantity_as_int / 5)
        remaining_items = quantity_as_int % 5

        discounted_total = number_of_sets * offer.argument + remaining_items * unit_price
        original_total = quantity * unit_price
        discount_amount = original_total - discounted_total

        if discount_amount <= 0:
            return None

        description = f"5 for {offer.argument:.2f}"
        return Discount(product, description, -discount_amount)

    def get_description(self):
        return "Buy 5 for special price"
