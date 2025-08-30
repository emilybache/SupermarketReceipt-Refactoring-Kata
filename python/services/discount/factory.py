from python.domain.model_objects import SpecialOfferType
from .strategies.three_for_two import ThreeForTwoStrategy
from .strategies.percentage import PercentageDiscountStrategy
from .strategies.two_for_amount import TwoForAmountStrategy
from .strategies.five_for_amount import FiveForAmountStrategy


class DiscountStrategyFactory:
    """Factory to create appropriate discount strategies"""

    _strategies = {
        SpecialOfferType.THREE_FOR_TWO: ThreeForTwoStrategy,
        SpecialOfferType.TEN_PERCENT_DISCOUNT: PercentageDiscountStrategy,
        SpecialOfferType.TWO_FOR_AMOUNT: TwoForAmountStrategy,
        SpecialOfferType.FIVE_FOR_AMOUNT: FiveForAmountStrategy,
    }

    @classmethod
    def create_strategy(cls, offer_type):
        """
        Create a discount strategy for the given offer type.

        Args:
            offer_type: SpecialOfferType enum value

        Returns:
            DiscountStrategy instance

        Raises:
            ValueError: If offer_type is not supported
        """
        strategy_class = cls._strategies.get(offer_type)
        if strategy_class:
            return strategy_class()
        raise ValueError(f"Unknown offer type: {offer_type}")

    @classmethod
    def get_supported_offer_types(cls):
        """Get list of all supported offer types"""
        return list(cls._strategies.keys())

    @classmethod
    def register_strategy(cls, offer_type, strategy_class):
        """
        Register a new strategy for an offer type.
        Useful for extending the system with new discount types.
        """
        cls._strategies[offer_type] = strategy_class
