from python.domain.receipt import Receipt
from python.domain.model_objects import Offer
from .pricing_service import PricingService
from .discount.discount_calculator import DiscountCalculator


class CheckoutService:
    """
    Main service for handling the checkout process.

    This service orchestrates:
    - Pricing calculations
    - Receipt generation
    - Discount applications
    - Order validation
    """

    def __init__(self, catalog):
        self.catalog = catalog
        self.pricing_service = PricingService(catalog)
        self.discount_calculator = DiscountCalculator()
        self.offers = {}

    def add_special_offer(self, offer_type, product, argument):
        """Add a special offer for a product"""
        self.offers[product] = Offer(offer_type, product, argument)

    def remove_special_offer(self, product):
        """Remove special offer for a product"""
        if product in self.offers:
            del self.offers[product]

    def get_special_offers(self):
        """Get all current special offers"""
        return dict(self.offers)

    def checkout(self, shopping_cart):
        """
        Process checkout for items in the shopping cart.

        Args:
            shopping_cart: ShoppingCart object containing items

        Returns:
            Receipt object with all items and applicable discounts
        """
        receipt = Receipt()

        # Add all items to receipt with prices
        self._add_items_to_receipt(receipt, shopping_cart.items)

        # Calculate and apply discounts
        product_quantities = self.pricing_service.aggregate_product_quantities(
            shopping_cart.items
        )

        self.discount_calculator.apply_discounts(
            receipt, product_quantities, self.offers, self.catalog
        )

        return receipt

    def _add_items_to_receipt(self, receipt, cart_items):
        """Add all items from cart to receipt with calculated prices"""
        for item in cart_items:
            product = item.product
            quantity = item.quantity

            # Validate inputs
            self.pricing_service.validate_pricing_inputs(product, quantity)

            # Calculate prices
            unit_price, total_price = self.pricing_service.calculate_line_item_price(
                product, quantity
            )

            # Add to receipt
            receipt.add_product(product, quantity, unit_price, total_price)

    def calculate_total_before_discounts(self, shopping_cart):
        """Calculate total price before any discounts are applied"""
        total = 0
        for item in shopping_cart.items:
            _, line_total = self.pricing_service.calculate_line_item_price(
                item.product, item.quantity
            )
            total += line_total
        return total

    def get_available_discount_types(self):
        """Get list of all available discount types"""
        return self.discount_calculator.get_available_discount_types()
