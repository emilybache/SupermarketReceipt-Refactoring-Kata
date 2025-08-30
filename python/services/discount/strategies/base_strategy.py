from abc import ABC, abstractmethod


class DiscountStrategy(ABC):
    """Abstract base class for discount calculation strategies"""

    @abstractmethod
    def calculate_discount(self, product, quantity, unit_price, offer):
        """
        Calculate discount for a product with given quantity and offer.

        Args:
            product: The product being discounted
            quantity: Quantity of the product
            unit_price: Price per unit of the product
            offer: The special offer details

        Returns:
            Discount object if discount applies, None otherwise
        """
        pass

    @abstractmethod
    def get_description(self):
        """Get a human-readable description of this discount strategy"""
        pass
