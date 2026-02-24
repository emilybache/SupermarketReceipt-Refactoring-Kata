import math
from .offer import Offer
from ..discount import Discount


class ThreeForTwoOffer(Offer):
    """Buy 3, pay for 2"""
    def calculate_discount(self, quantity, unit_price):
        quantity_as_int = int(quantity)
        if quantity_as_int > 2:
            number_of_sets = math.floor(quantity_as_int / 3)
            discount_amount = quantity * unit_price - (
                (number_of_sets * 2 * unit_price) + quantity_as_int % 3 * unit_price)
            return Discount(self.product, "3 for 2", -discount_amount)
        return None
