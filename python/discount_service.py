from .discount_strategies import DiscountStrategyFactory


class DiscountService:
    """Service responsible for calculating all discounts for a receipt"""

    def __init__(self):
        self.strategy_factory = DiscountStrategyFactory()

    def apply_discounts(self, receipt, product_quantities, offers, catalog):
        """Apply all applicable discounts to the receipt"""
        for product, quantity in product_quantities.items():
            if product in offers:
                offer = offers[product]
                unit_price = catalog.unit_price(product)

                discount = self._calculate_discount(product, quantity, unit_price, offer)
                if discount:
                    receipt.add_discount(discount)

    def _calculate_discount(self, product, quantity, unit_price, offer):
        """Calculate discount for a single product using appropriate strategy"""
        try:
            strategy = self.strategy_factory.create_strategy(offer.offer_type)
            return strategy.calculate_discount(product, quantity, unit_price, offer)
        except ValueError:
            # Unknown offer type - skip discount
            return None
