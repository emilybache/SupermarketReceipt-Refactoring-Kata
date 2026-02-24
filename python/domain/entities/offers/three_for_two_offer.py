import math
from .offer import Offer
from ..discount import Discount


class ThreeForTwoOffer(Offer):
    """Buy 3, pay for 2"""
    
    def __init__(self, product):
        self.product = product
    
    def applies_to_product(self, product):
        return product == self.product
    
    def calculate_discount(self, cart, catalog):
        if self.product not in cart.product_quantities:
            return None
        
        quantity = cart.product_quantities[self.product]
        unit_price = catalog.unit_price(self.product)
        quantity_as_int = int(quantity)
        
        if quantity_as_int > 2:
            number_of_sets = math.floor(quantity_as_int / 3)
            discount_amount = quantity * unit_price - (
                (number_of_sets * 2 * unit_price) + quantity_as_int % 3 * unit_price)
            return Discount(self.product, "3 for 2", -discount_amount)
        return None
