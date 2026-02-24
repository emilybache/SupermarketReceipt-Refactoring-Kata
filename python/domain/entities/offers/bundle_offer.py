from .offer import Offer
from ..discount import Discount


class BundleOffer(Offer):
    """Bundle discount - buy all items in bundle, get percentage off total"""
    
    def __init__(self, products, discount_percentage):
        self.products = products  # List of products in the bundle
        self.discount_percentage = discount_percentage
    
    def applies_to_product(self, product):
        """Bundle offer applies to all products in the bundle"""
        return product in self.products
    
    def calculate_discount(self, cart, catalog):
        """Calculate bundle discount based on complete bundles in cart.
        
        Args:
            cart: ShoppingCart - the shopping cart with product quantities
            catalog: SupermarketCatalog - to get unit prices
            
        Returns:
            Discount object or None if no complete bundle exists
        """
        # Find how many complete bundles we can make
        complete_bundles = self._count_complete_bundles(cart.product_quantities)
        
        if complete_bundles == 0:
            return None
        
        # Calculate total price of items in bundles
        bundle_total = 0
        for product in self.products:
            unit_price = catalog.unit_price(product)
            bundle_total += unit_price * complete_bundles
        
        # Calculate discount
        discount_amount = bundle_total * self.discount_percentage / 100.0
        
        # Create description listing all products in bundle
        product_names = ", ".join(p.name for p in self.products)
        description = f"{self.discount_percentage}% off bundle"
        
        # Return discount with the first product as reference
        return Discount(self.products[0], description, -discount_amount)
    
    def _count_complete_bundles(self, product_quantities):
        """Count how many complete bundles can be formed from cart contents"""
        if not all(product in product_quantities for product in self.products):
            return 0
        
        # Find minimum quantity across all products in bundle
        min_quantity = min(int(product_quantities[product]) for product in self.products)
        return min_quantity
