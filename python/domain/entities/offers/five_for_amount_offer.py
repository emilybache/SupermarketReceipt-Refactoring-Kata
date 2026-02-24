import math
from .offer import Offer
from ..discount import Discount


class FiveForAmountOffer(Offer):
    """Buy 5 for a special price"""
    def calculate_discount(self, quantity, unit_price):
        quantity_as_int = int(quantity)
        if quantity_as_int >= 5:
            number_of_sets = math.floor(quantity_as_int / 5)
            discount_total = unit_price * quantity - (
                self.argument * number_of_sets + quantity_as_int % 5 * unit_price)
            return Discount(self.product, "5 for " + str(self.argument), -discount_total)
        return None
