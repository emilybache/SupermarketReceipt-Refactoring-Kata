from .offer import Offer
from ..discount import Discount


class TenPercentDiscountOffer(Offer):
    """Percentage discount on product"""
    def calculate_discount(self, quantity, unit_price):
        discount_amount = quantity * unit_price * self.argument / 100.0
        return Discount(self.product, str(self.argument) + "% off", -discount_amount)
