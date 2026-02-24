from .offer import Offer
from ..discount import Discount


class TwoForAmountOffer(Offer):
    """Buy 2 for a special price"""
    def calculate_discount(self, quantity, unit_price):
        quantity_as_int = int(quantity)
        if quantity_as_int >= 2:
            number_of_sets = quantity_as_int // 2
            total = self.argument * number_of_sets + (quantity_as_int % 2) * unit_price
            discount_amount = unit_price * quantity - total
            return Discount(self.product, "2 for " + str(self.argument), -discount_amount)
        return None
