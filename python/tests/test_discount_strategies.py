import pytest
from ..model_objects import Product, ProductUnit, Offer, SpecialOfferType
from ..discount_strategies import (
    ThreeForTwoStrategy,
    TenPercentDiscountStrategy,
    TwoForAmountStrategy,
    FiveForAmountStrategy,
    DiscountStrategyFactory
)


class TestDiscountStrategies:
    """Test individual discount strategies"""

    def setup_method(self):
        self.toothbrush = Product("toothbrush", ProductUnit.EACH)
        self.unit_price = 0.99

    def test_three_for_two_strategy_exact_quantity(self):
        strategy = ThreeForTwoStrategy()
        offer = Offer(SpecialOfferType.THREE_FOR_TWO, self.toothbrush, 0)

        discount = strategy.calculate_discount(self.toothbrush, 3, self.unit_price, offer)

        assert discount is not None
        assert discount.description == "3 for 2"
        expected_discount = 3 * self.unit_price - 2 * self.unit_price
        assert discount.discount_amount == pytest.approx(-expected_discount, 0.01)

    def test_three_for_two_strategy_insufficient_quantity(self):
        strategy = ThreeForTwoStrategy()
        offer = Offer(SpecialOfferType.THREE_FOR_TWO, self.toothbrush, 0)

        discount = strategy.calculate_discount(self.toothbrush, 2, self.unit_price, offer)

        assert discount is None

    def test_three_for_two_strategy_seven_items(self):
        strategy = ThreeForTwoStrategy()
        offer = Offer(SpecialOfferType.THREE_FOR_TWO, self.toothbrush, 0)

        discount = strategy.calculate_discount(self.toothbrush, 7, self.unit_price, offer)

        assert discount is not None
        # 7 items: 6 items = 2 sets of 3-for-2 (pay for 4) + 1 regular item = pay for 5
        expected_discount = 7 * self.unit_price - 5 * self.unit_price
        assert discount.discount_amount == pytest.approx(-expected_discount, 0.01)

    def test_ten_percent_discount_strategy(self):
        strategy = TenPercentDiscountStrategy()
        offer = Offer(SpecialOfferType.TEN_PERCENT_DISCOUNT, self.toothbrush, 20.0)

        discount = strategy.calculate_discount(self.toothbrush, 2, self.unit_price, offer)

        assert discount is not None
        assert discount.description == "20.0% off"
        expected_discount = 2 * self.unit_price * 0.20
        assert discount.discount_amount == pytest.approx(-expected_discount, 0.01)

    def test_two_for_amount_strategy(self):
        strategy = TwoForAmountStrategy()
        offer = Offer(SpecialOfferType.TWO_FOR_AMOUNT, self.toothbrush, 1.50)

        discount = strategy.calculate_discount(self.toothbrush, 2, self.unit_price, offer)

        assert discount is not None
        assert discount.description == "2 for 1.5"
        expected_discount = 2 * self.unit_price - 1.50
        assert discount.discount_amount == pytest.approx(-expected_discount, 0.01)

    def test_two_for_amount_strategy_insufficient_quantity(self):
        strategy = TwoForAmountStrategy()
        offer = Offer(SpecialOfferType.TWO_FOR_AMOUNT, self.toothbrush, 1.50)

        discount = strategy.calculate_discount(self.toothbrush, 1, self.unit_price, offer)

        assert discount is None

    def test_five_for_amount_strategy(self):
        strategy = FiveForAmountStrategy()
        offer = Offer(SpecialOfferType.FIVE_FOR_AMOUNT, self.toothbrush, 4.00)

        discount = strategy.calculate_discount(self.toothbrush, 5, self.unit_price, offer)

        assert discount is not None
        assert discount.description == "5 for 4.0"
        expected_discount = 5 * self.unit_price - 4.00
        assert discount.discount_amount == pytest.approx(-expected_discount, 0.01)

    def test_five_for_amount_strategy_with_extra_items(self):
        strategy = FiveForAmountStrategy()
        offer = Offer(SpecialOfferType.FIVE_FOR_AMOUNT, self.toothbrush, 4.00)

        discount = strategy.calculate_discount(self.toothbrush, 7, self.unit_price, offer)

        assert discount is not None
        # 7 items: 5 for 4.00 + 2 at regular price = 4.00 + 2*0.99
        expected_total = 4.00 + 2 * self.unit_price
        expected_discount = 7 * self.unit_price - expected_total
        assert discount.discount_amount == pytest.approx(-expected_discount, 0.01)


class TestDiscountStrategyFactory:
    """Test the discount strategy factory"""

    def test_create_three_for_two_strategy(self):
        strategy = DiscountStrategyFactory.create_strategy(SpecialOfferType.THREE_FOR_TWO)
        assert isinstance(strategy, ThreeForTwoStrategy)

    def test_create_ten_percent_strategy(self):
        strategy = DiscountStrategyFactory.create_strategy(SpecialOfferType.TEN_PERCENT_DISCOUNT)
        assert isinstance(strategy, TenPercentDiscountStrategy)

    def test_create_two_for_amount_strategy(self):
        strategy = DiscountStrategyFactory.create_strategy(SpecialOfferType.TWO_FOR_AMOUNT)
        assert isinstance(strategy, TwoForAmountStrategy)

    def test_create_five_for_amount_strategy(self):
        strategy = DiscountStrategyFactory.create_strategy(SpecialOfferType.FIVE_FOR_AMOUNT)
        assert isinstance(strategy, FiveForAmountStrategy)

    def test_unknown_strategy_type_raises_error(self):
        with pytest.raises(ValueError, match="Unknown offer type"):
            DiscountStrategyFactory.create_strategy("UNKNOWN_TYPE")
