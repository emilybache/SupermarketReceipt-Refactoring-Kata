import math
from .offer import Offer
from ..discount import Discount


class FiveForAmountOffer(Offer):
    """Buy 5 for a special price"""
    
    def __init__(self, product, special_price):
        self.product = product
        self.special_price = special_price
    
    def applies_to_product(self, product):
        return product == self.product
    
    def calculate_discount(self, cart, catalog):
        if self.product not in cart.product_quantities:
            return None
        
        quantity = cart.product_quantities[self.product]
        unit_price = catalog.unit_price(self.product)
        quantity_as_int = int(quantity)
        
        if quantity_as_int >= 5:
            number_of_sets = math.floor(quantity_as_int / 5)
            discount_total = unit_price * quantity - (
                self.special_price * number_of_sets + quantity_as_int % 5 * unit_price)
            return Discount(self.product, "5 for " + str(self.special_price), -discount_total)
        return None
