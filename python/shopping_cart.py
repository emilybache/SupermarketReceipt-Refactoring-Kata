from .model_objects import ProductQuantity
from .discount_service import DiscountService


class ShoppingCart:
    """Represents a shopping cart containing items to be purchased"""

    def __init__(self):
        self._items = []
        self._product_quantities = {}
        self._discount_service = DiscountService()

    @property
    def items(self):
        """Get list of all items in the cart"""
        return self._items

    @property
    def product_quantities(self):
        """Get dictionary of products and their total quantities"""
        return self._product_quantities

    def add_item(self, product):
        """Add a single item to the cart"""
        self.add_item_quantity(product, 1.0)

    def add_item_quantity(self, product, quantity):
        """Add a product with specified quantity to the cart"""
        if quantity < 0:
            raise ValueError("Quantity cannot be negative")

        self._items.append(ProductQuantity(product, quantity))

        if product in self._product_quantities:
            self._product_quantities[product] += quantity
        else:
            self._product_quantities[product] = quantity

    def apply_discounts(self, receipt, offers, catalog):
        """Apply all applicable discounts to the receipt"""
        self._discount_service.apply_discounts(
            receipt,
            self._product_quantities,
            offers,
            catalog
        )
