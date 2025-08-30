import pytest
from python.domain.model_objects import Product, ProductUnit, Offer, SpecialOfferType
from python.services.discount.strategies.three_for_two import ThreeForTwoStrategy
from python.services.discount.strategies.percentage import PercentageDiscountStrategy
from python.services.discount.strategies.two_for_amount import TwoForAmountStrategy
from python.services.discount.strategies.five_for_amount import FiveForAmountStrategy
from python.services.discount.factory import DiscountStrategyFactory


class TestThreeForTwoStrategy:
    """Test the 3-for-2 discount strategy"""

    def setup_method(self):
        self.strategy = ThreeForTwoStrategy()
        self.toothbrush = Product("toothbrush", ProductUnit.EACH)
        self.offer = Offer(SpecialOfferType.THREE_FOR_TWO, self.toothbrush, 0)
        self.unit_price = 0.99

    def test_exact_quantity_three_items(self):
        discount = self.strategy.calculate_discount(
            self.toothbrush, 3, self.unit_price, self.offer
        )

        assert discount is not None
        assert discount.description == "3 for 2"
        # Should pay for 2, get 1 free: 3 * 0.99 - 2 * 0.99 = 0.99 discount
        expected_discount = 0.99
        assert discount.discount_amount == pytest.approx(-expected_discount, 0.01)

    def test_insufficient_quantity_two_items(self):
        discount = self.strategy.calculate_discount(
            self.toothbrush, 2, self.unit_price, self.offer
        )

        assert discount is None

    def test_seven_items_two_sets_plus_one(self):
        discount = self.strategy.calculate_discount(
            self.toothbrush, 7, self.unit_price, self.offer
        )

        assert discount is not None
        # 7 items: 6 items = 2 sets of 3-for-2 (pay for 4) + 1 regular = pay for 5 total
        # Discount = 7 * 0.99 - 5 * 0.99 = 2 * 0.99
        expected_discount = 2 * self.unit_price
        assert discount.discount_amount == pytest.approx(-expected_discount, 0.01)

    def test_six_items_exact_two_sets(self):
        discount = self.strategy.calculate_discount(
            self.toothbrush, 6, self.unit_price, self.offer
        )

        assert discount is not None
        # 6 items: 2 sets of 3-for-2 = pay for 4, get 2 free
        expected_discount = 2 * self.unit_price
        assert discount.discount_amount == pytest.approx(-expected_discount, 0.01)


class TestPercentageDiscountStrategy:
    """Test the percentage discount strategy"""

    def setup_method(self):
        self.strategy = PercentageDiscountStrategy()
        self.apples = Product("apples", ProductUnit.KILO)
        self.unit_price = 1.99

    def test_twenty_percent_discount(self):
        offer = Offer(SpecialOfferType.TEN_PERCENT_DISCOUNT, self.apples, 20.0)
        discount = self.strategy.calculate_discount(
            self.apples, 2.5, self.unit_price, offer
        )

        assert discount is not None
        assert discount.description == "20.0% off"
        # 2.5 * 1.99 * 0.20 = 0.995
        expected_discount = 2.5 * self.unit_price * 0.20
        assert discount.discount_amount == pytest.approx(-expected_discount, 0.01)

    def test_ten_percent_discount(self):
        offer = Offer(SpecialOfferType.TEN_PERCENT_DISCOUNT, self.apples, 10.0)
        discount = self.strategy.calculate_discount(
            self.apples, 1.0, self.unit_price, offer
        )

        assert discount is not None
        assert discount.description == "10.0% off"
        expected_discount = 1.0 * self.unit_price * 0.10
        assert discount.discount_amount == pytest.approx(-expected_discount, 0.01)

    def test_invalid_negative_percentage(self):
        offer = Offer(SpecialOfferType.TEN_PERCENT_DISCOUNT, self.apples, -10.0)
        discount = self.strategy.calculate_discount(
            self.apples, 1.0, self.unit_price, offer
        )

        assert discount is None

    def test_invalid_hundred_percent(self):
        offer = Offer(SpecialOfferType.TEN_PERCENT_DISCOUNT, self.apples, 100.0)
        discount = self.strategy.calculate_discount(
            self.apples, 1.0, self.unit_price, offer
        )

        assert discount is None


class TestTwoForAmountStrategy:
    """Test the 2-for-amount discount strategy"""

    def setup_method(self):
        self.strategy = TwoForAmountStrategy()
        self.cherry_tomatoes = Product("cherry tomatoes", ProductUnit.EACH)
        self.unit_price = 0.69

    def test_exact_two_items(self):
        offer = Offer(SpecialOfferType.TWO_FOR_AMOUNT, self.cherry_tomatoes, 0.99)
        discount = self.strategy.calculate_discount(
            self.cherry_tomatoes, 2, self.unit_price, offer
        )

        assert discount is not None
        assert discount.description == "2 for 0.99"
        # Original: 2 * 0.69 = 1.38, New: 0.99, Discount: 1.38 - 0.99 = 0.39
        expected_discount = 2 * self.unit_price - 0.99
        assert discount.discount_amount == pytest.approx(-expected_discount, 0.01)

    def test_insufficient_one_item(self):
        offer = Offer(SpecialOfferType.TWO_FOR_AMOUNT, self.cherry_tomatoes, 0.99)
        discount = self.strategy.calculate_discount(
            self.cherry_tomatoes, 1, self.unit_price, offer
        )

        assert discount is None

    def test_three_items_one_pair_plus_one(self):
        offer = Offer(SpecialOfferType.TWO_FOR_AMOUNT, self.cherry_tomatoes, 0.99)
        discount = self.strategy.calculate_discount(
            self.cherry_tomatoes, 3, self.unit_price, offer
        )

        assert discount is not None
        # 3 items: 2 for 0.99 + 1 at regular price = 0.99 + 0.69 = 1.68
        # Original: 3 * 0.69 = 2.07, Discount: 2.07 - 1.68 = 0.39
        expected_total = 0.99 + self.unit_price
        original_total = 3 * self.unit_price
        expected_discount = original_total - expected_total
        assert discount.discount_amount == pytest.approx(-expected_discount, 0.01)


class TestFiveForAmountStrategy:
    """Test the 5-for-amount discount strategy"""

    def setup_method(self):
        self.strategy = FiveForAmountStrategy()
        self.toothpaste = Product("toothpaste", ProductUnit.EACH)
        self.unit_price = 1.79

    def test_exact_five_items(self):
        offer = Offer(SpecialOfferType.FIVE_FOR_AMOUNT, self.toothpaste, 7.49)
        discount = self.strategy.calculate_discount(
            self.toothpaste, 5, self.unit_price, offer
        )

        assert discount is not None
        assert discount.description == "5 for 7.49"
        # Original: 5 * 1.79 = 8.95, New: 7.49, Discount: 8.95 - 7.49 = 1.46
        expected_discount = 5 * self.unit_price - 7.49
        assert discount.discount_amount == pytest.approx(-expected_discount, 0.01)

    def test_insufficient_four_items(self):
        offer = Offer(SpecialOfferType.FIVE_FOR_AMOUNT, self.toothpaste, 7.49)
        discount = self.strategy.calculate_discount(
            self.toothpaste, 4, self.unit_price, offer
        )

        assert discount is None

    def test_seven_items_one_set_plus_two(self):
        offer = Offer(SpecialOfferType.FIVE_FOR_AMOUNT, self.toothpaste, 7.49)
        discount = self.strategy.calculate_discount(
            self.toothpaste, 7, self.unit_price, offer
        )

        assert discount is not None
        # 7 items: 5 for 7.49 + 2 at regular price = 7.49 + 2 * 1.79 = 11.07
        # Original: 7 * 1.79 = 12.53, Discount: 12.53 - 11.07 = 1.46
        expected_total = 7.49 + 2 * self.unit_price
        original_total = 7 * self.unit_price
        expected_discount = original_total - expected_total
        assert discount.discount_amount == pytest.approx(-expected_discount, 0.01)


class TestDiscountStrategyFactory:
    """Test the discount strategy factory"""

    def test_create_three_for_two_strategy(self):
        strategy = DiscountStrategyFactory.create_strategy(SpecialOfferType.THREE_FOR_TWO)
        assert isinstance(strategy, ThreeForTwoStrategy)

    def test_create_percentage_strategy(self):
        strategy = DiscountStrategyFactory.create_strategy(SpecialOfferType.TEN_PERCENT_DISCOUNT)
        assert isinstance(strategy, PercentageDiscountStrategy)

    def test_create_two_for_amount_strategy(self):
        strategy = DiscountStrategyFactory.create_strategy(SpecialOfferType.TWO_FOR_AMOUNT)
        assert isinstance(strategy, TwoForAmountStrategy)

    def test_create_five_for_amount_strategy(self):
        strategy = DiscountStrategyFactory.create_strategy(SpecialOfferType.FIVE_FOR_AMOUNT)
        assert isinstance(strategy, FiveForAmountStrategy)

    def test_unknown_strategy_raises_error(self):
        with pytest.raises(ValueError, match="Unknown offer type"):
            DiscountStrategyFactory.create_strategy("UNKNOWN_TYPE")

    def test_get_supported_offer_types(self):
        types = DiscountStrategyFactory.get_supported_offer_types()
        assert SpecialOfferType.THREE_FOR_TWO in types
        assert SpecialOfferType.TEN_PERCENT_DISCOUNT in types
        assert SpecialOfferType.TWO_FOR_AMOUNT in types
        assert SpecialOfferType.FIVE_FOR_AMOUNT in types
