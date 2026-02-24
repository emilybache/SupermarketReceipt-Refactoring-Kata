from .offer import Offer
from ..discount import Discount


class TwoForAmountOffer(Offer):
    """Buy 2 for a special price"""
    
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
        
        if quantity_as_int >= 2:
            number_of_sets = quantity_as_int // 2
            total = self.special_price * number_of_sets + (quantity_as_int % 2) * unit_price
            discount_amount = unit_price * quantity - total
            return Discount(self.product, "2 for " + str(self.special_price), -discount_amount)
        return None
