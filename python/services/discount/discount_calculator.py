from .factory import DiscountStrategyFactory


class DiscountCalculator:
    """
    Main service for calculating discounts.

    This class coordinates the discount calculation process by:
    1. Determining which products have offers
    2. Getting the appropriate strategy for each offer type
    3. Calculating discounts using the strategies
    4. Applying valid discounts to the receipt
    """

    def __init__(self):
        self.strategy_factory = DiscountStrategyFactory()

    def apply_discounts(self, receipt, product_quantities, offers, catalog):
        """
        Apply all applicable discounts to the receipt.

        Args:
            receipt: Receipt object to add discounts to
            product_quantities: Dict mapping products to their total quantities
            offers: Dict mapping products to their special offers
            catalog: Catalog to get unit prices from
        """
        for product, quantity in product_quantities.items():
            if product in offers:
                offer = offers[product]
                unit_price = catalog.unit_price(product)

                discount = self._calculate_discount_for_product(
                    product, quantity, unit_price, offer
                )

                if discount:
                    receipt.add_discount(discount)

    def _calculate_discount_for_product(self, product, quantity, unit_price, offer):
        """
        Calculate discount for a single product using the appropriate strategy.

        Returns:
            Discount object if discount applies, None otherwise
        """
        try:
            strategy = self.strategy_factory.create_strategy(offer.offer_type)
            return strategy.calculate_discount(product, quantity, unit_price, offer)
        except ValueError as e:
            # Log the error in a real application
            print(f"Warning: {e}")
            return None

    def get_available_discount_types(self):
        """Get list of all available discount types"""
        return self.strategy_factory.get_supported_offer_types()
