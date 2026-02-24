from abc import ABC, abstractmethod


class Offer(ABC):
    """Abstract base class for all offer types"""
    def __init__(self, product, argument):
        self.product = product
        self.argument = argument
    
    @abstractmethod
    def calculate_discount(self, quantity, unit_price):
        """Calculate discount for given quantity and unit price.
        Returns a Discount object or None if no discount applies."""
        pass
