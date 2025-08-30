from abc import ABC, abstractmethod
import math
from .model_objects import Discount, SpecialOfferType


class DiscountStrategy(ABC):
    """Abstract base class for discount calculation strategies"""

    @abstractmethod
    def calculate_discount(self, product, quantity, unit_price, offer):
        """Calculate discount for a product with given quantity and offer"""
        pass


class ThreeForTwoStrategy(DiscountStrategy):
    """Buy 3, pay for 2 discount strategy"""

    def calculate_discount(self, product, quantity, unit_price, offer):
        quantity_as_int = int(quantity)
        if quantity_as_int <= 2:
            return None

        number_of_sets = math.floor(quantity_as_int / 3)
        discount_amount = quantity * unit_price - (
                (number_of_sets * 2 * unit_price) + (quantity_as_int % 3) * unit_price
        )

        return Discount(product, "3 for 2", -discount_amount)


class TenPercentDiscountStrategy(DiscountStrategy):
    """Percentage discount strategy"""

    def calculate_discount(self, product, quantity, unit_price, offer):
        discount_amount = quantity * unit_price * offer.argument / 100.0
        description = f"{offer.argument}% off"
        return Discount(product, description, -discount_amount)


class TwoForAmountStrategy(DiscountStrategy):
    """Buy 2 for a specific amount strategy"""

    def calculate_discount(self, product, quantity, unit_price, offer):
        quantity_as_int = int(quantity)
        if quantity_as_int < 2:
            return None

        total = offer.argument * (quantity_as_int // 2) + (quantity_as_int % 2) * unit_price
        discount_amount = unit_price * quantity - total
        description = f"2 for {offer.argument}"

        return Discount(product, description, -discount_amount)


class FiveForAmountStrategy(DiscountStrategy):
    """Buy 5 for a specific amount strategy"""

    def calculate_discount(self, product, quantity, unit_price, offer):
        quantity_as_int = int(quantity)
        if quantity_as_int < 5:
            return None

        number_of_sets = math.floor(quantity_as_int / 5)
        total = offer.argument * number_of_sets + (quantity_as_int % 5) * unit_price
        discount_amount = unit_price * quantity - total
        description = f"5 for {offer.argument}"

        return Discount(product, description, -discount_amount)


class DiscountStrategyFactory:
    """Factory to create appropriate discount strategies"""

    _strategies = {
        SpecialOfferType.THREE_FOR_TWO: ThreeForTwoStrategy,
        SpecialOfferType.TEN_PERCENT_DISCOUNT: TenPercentDiscountStrategy,
        SpecialOfferType.TWO_FOR_AMOUNT: TwoForAmountStrategy,
        SpecialOfferType.FIVE_FOR_AMOUNT: FiveForAmountStrategy,
    }

    @classmethod
    def create_strategy(cls, offer_type):
        """Create a discount strategy for the given offer type"""
        strategy_class = cls._strategies.get(offer_type)
        if strategy_class:
            return strategy_class()
        raise ValueError(f"Unknown offer type: {offer_type}")
