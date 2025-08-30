from python.services.checkout_service import CheckoutService


class Teller:
    """
    Handles the checkout process at the point of sale.

    This class serves as a facade for the checkout process,
    delegating the actual business logic to the CheckoutService.
    """

    def __init__(self, catalog):
        self.checkout_service = CheckoutService(catalog)

    def add_special_offer(self, offer_type, product, argument):
        """
        Add a special offer for a product.

        Args:
            offer_type: SpecialOfferType enum value
            product: Product to apply the offer to
            argument: Offer-specific argument (percentage, amount, etc.)
        """
        self.checkout_service.add_special_offer(offer_type, product, argument)

    def remove_special_offer(self, product):
        """
        Remove special offer for a product.

        Args:
            product: Product to remove offer from
        """
        self.checkout_service.remove_special_offer(product)

    def get_special_offers(self):
        """Get all current special offers"""
        return self.checkout_service.get_special_offers()

    def checks_out_articles_from(self, shopping_cart):
        """
        Process checkout for items in the shopping cart.

        This is the main method that processes a shopping cart
        and returns a receipt with all items and applicable discounts.

        Args:
            shopping_cart: ShoppingCart object containing items to purchase

        Returns:
            Receipt object with all items, discounts, and total
        """
        return self.checkout_service.checkout(shopping_cart)

    def calculate_total_before_discounts(self, shopping_cart):
        """
        Calculate total price before any discounts are applied.

        Useful for showing customers how much they're saving.

        Args:
            shopping_cart: ShoppingCart object

        Returns:
            float: Total price before discounts
        """
        return self.checkout_service.calculate_total_before_discounts(shopping_cart)

    def get_available_discount_types(self):
        """Get list of all available discount types"""
        return self.checkout_service.get_available_discount_types()
