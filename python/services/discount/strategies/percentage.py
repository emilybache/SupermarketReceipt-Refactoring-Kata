from python.domain.model_objects import Discount
from .base_strategy import DiscountStrategy


class PercentageDiscountStrategy(DiscountStrategy):
    """Percentage discount strategy (e.g., 10% off, 20% off)"""

    def calculate_discount(self, product, quantity, unit_price, offer):
        if offer.argument <= 0 or offer.argument >= 100:
            return None

        discount_amount = quantity * unit_price * offer.argument / 100.0
        description = f"{offer.argument}% off"

        return Discount(product, description, -discount_amount)

    def get_description(self):
        return "Percentage discount"
