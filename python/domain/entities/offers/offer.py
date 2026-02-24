from abc import ABC, abstractmethod


class Offer(ABC):
    """Abstract base class for all offer types"""
    
    @abstractmethod
    def calculate_discount(self, cart, catalog):
        """Calculate discount based on cart contents and catalog prices.
        
        Args:
            cart: ShoppingCart - the shopping cart with product quantities
            catalog: SupermarketCatalog - to get unit prices
            
        Returns:
            Discount object or None if no discount applies.
        """
        pass
    
    @abstractmethod
    def applies_to_product(self, product):
        """Check if this offer applies to the given product.
        
        Args:
            product: Product to check
            
        Returns:
            bool - True if offer applies to this product
        """
        pass
