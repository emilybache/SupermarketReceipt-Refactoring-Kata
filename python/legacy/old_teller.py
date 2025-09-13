from python.domain.model_objects import Offer
from python.domain.receipt import Receipt


class Teller:
    """Handles the checkout process"""

    def __init__(self, catalog):
        self.catalog = catalog
        self.offers = {}

    def add_special_offer(self, offer_type, product, argument):
        """Add a special offer for a product"""
        self.offers[product] = Offer(offer_type, product, argument)

    def checks_out_articles_from(self, shopping_cart):
        """Process checkout for items in the shopping cart"""
        receipt = Receipt()

        # Add all items to receipt
        self._add_items_to_receipt(receipt, shopping_cart.items)

        # Apply discounts
        shopping_cart.apply_discounts(receipt, self.offers, self.catalog)

        return receipt

    def _add_items_to_receipt(self, receipt, items):
        """Add all items from cart to receipt with prices"""
        for product_quantity in items:
            product = product_quantity.product
            quantity = product_quantity.quantity
            unit_price = self.catalog.unit_price(product)
            total_price = quantity * unit_price

            receipt.add_product(product, quantity, unit_price, total_price)
