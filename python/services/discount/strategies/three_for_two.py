import math
from python.domain.model_objects import Discount
from .base_strategy import DiscountStrategy


class ThreeForTwoStrategy(DiscountStrategy):
    """Buy 3, pay for 2 discount strategy"""

    def calculate_discount(self, product, quantity, unit_price, offer):
        quantity_as_int = int(quantity)
        if quantity_as_int < 3:
            return None

        number_of_sets = math.floor(quantity_as_int / 3)
        discount_amount = quantity * unit_price - (
                (number_of_sets * 2 * unit_price) + (quantity_as_int % 3) * unit_price
        )

        if discount_amount <= 0:
            return None

        return Discount(product, "3 for 2", -discount_amount)

    def get_description(self):
        return "Buy 3, pay for 2"
