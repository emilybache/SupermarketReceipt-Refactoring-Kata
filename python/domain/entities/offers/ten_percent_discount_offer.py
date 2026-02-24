from .offer import Offer
from ..discount import Discount


class TenPercentDiscountOffer(Offer):
    """Percentage discount on product"""
    
    def __init__(self, product, percentage):
        self.product = product
        self.percentage = percentage
    
    def applies_to_product(self, product):
        return product == self.product
    
    def calculate_discount(self, cart, catalog):
        if self.product not in cart.product_quantities:
            return None
        
        quantity = cart.product_quantities[self.product]
        unit_price = catalog.unit_price(self.product)
        discount_amount = quantity * unit_price * self.percentage / 100.0
        return Discount(self.product, str(self.percentage) + "% off", -discount_amount)
