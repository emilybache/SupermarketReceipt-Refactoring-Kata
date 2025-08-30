import pytest

from ..model_objects import Product, SpecialOfferType, ProductUnit
from ..shopping_cart import ShoppingCart
from ..teller import Teller
from .fake_catalog import FakeCatalog


class TestSpecialOffers:
    """Test various special offer types"""

    def setup_method(self):
        self.catalog = FakeCatalog()
        self.teller = Teller(self.catalog)

        self.toothbrush = Product("toothbrush", ProductUnit.EACH)
        self.apples = Product("apples", ProductUnit.KILO)
        self.rice = Product("rice", ProductUnit.EACH)
        self.toothpaste = Product("toothpaste", ProductUnit.EACH)
        self.cherry_tomatoes = Product("cherry tomatoes", ProductUnit.EACH)

        self.catalog.add_product(self.toothbrush, 0.99)
        self.catalog.add_product(self.apples, 1.99)
        self.catalog.add_product(self.rice, 2.49)
        self.catalog.add_product(self.toothpaste, 1.79)
        self.catalog.add_product(self.cherry_tomatoes, 0.69)

    def test_ten_percent_discount_applies_correctly(self):
        # Test the actual discount functionality
        self.teller.add_special_offer(SpecialOfferType.TEN_PERCENT_DISCOUNT, self.apples, 20.0)

        cart = ShoppingCart()
        cart.add_item_quantity(self.apples, 2.5)

        receipt = self.teller.checks_out_articles_from(cart)

        original_price = 2.5 * 1.99
        discount_amount = original_price * 0.20
        expected_total = original_price - discount_amount

        assert receipt.total_price() == pytest.approx(expected_total, 0.01)
        assert len(receipt.discounts) == 1
        assert receipt.discounts[0].product == self.apples
        assert "20.0% off" in receipt.discounts[0].description

    def test_three_for_two_exact_quantity(self):
        self.teller.add_special_offer(SpecialOfferType.THREE_FOR_TWO, self.toothbrush, 0)

        cart = ShoppingCart()
        cart.add_item_quantity(self.toothbrush, 3)

        receipt = self.teller.checks_out_articles_from(cart)

        # Should pay for 2, get 1 free
        expected_total = 2 * 0.99
        assert receipt.total_price() == pytest.approx(expected_total, 0.01)
        assert len(receipt.discounts) == 1
        assert receipt.discounts[0].description == "3 for 2"

    def test_three_for_two_more_than_three(self):
        self.teller.add_special_offer(SpecialOfferType.THREE_FOR_TWO, self.toothbrush, 0)

        cart = ShoppingCart()
        cart.add_item_quantity(self.toothbrush, 7)

        receipt = self.teller.checks_out_articles_from(cart)

        # 7 items: 6 items = 2 sets of 3-for-2 (pay for 4) + 1 regular item
        # So pay for 4 + 1 = 5 items
        expected_total = 5 * 0.99
        assert receipt.total_price() == pytest.approx(expected_total, 0.01)

    def test_three_for_two_less_than_three(self):
        self.teller.add_special_offer(SpecialOfferType.THREE_FOR_TWO, self.toothbrush, 0)

        cart = ShoppingCart()
        cart.add_item_quantity(self.toothbrush, 2)

        receipt = self.teller.checks_out_articles_from(cart)

        # Not enough items for discount
        expected_total = 2 * 0.99
        assert receipt.total_price() == pytest.approx(expected_total, 0.01)
        assert len(receipt.discounts) == 0

    def test_two_for_amount_offer(self):
        # Two cherry tomatoes for €0.99
        self.teller.add_special_offer(SpecialOfferType.TWO_FOR_AMOUNT, self.cherry_tomatoes, 0.99)

        cart = ShoppingCart()
        cart.add_item_quantity(self.cherry_tomatoes, 2)

        receipt = self.teller.checks_out_articles_from(cart)

        assert receipt.total_price() == pytest.approx(0.99, 0.01)
        assert len(receipt.discounts) == 1

    def test_five_for_amount_offer(self):
        # Five tubes of toothpaste for €7.49
        self.teller.add_special_offer(SpecialOfferType.FIVE_FOR_AMOUNT, self.toothpaste, 7.49)

        cart = ShoppingCart()
        cart.add_item_quantity(self.toothpaste, 5)

        receipt = self.teller.checks_out_articles_from(cart)

        assert receipt.total_price() == pytest.approx(7.49, 0.01)
        assert len(receipt.discounts) == 1

    def test_five_for_amount_with_extra_items(self):
        self.teller.add_special_offer(SpecialOfferType.FIVE_FOR_AMOUNT, self.toothpaste, 7.49)

        cart = ShoppingCart()
        cart.add_item_quantity(self.toothpaste, 7)

        receipt = self.teller.checks_out_articles_from(cart)

        # 5 for 7.49 + 2 at regular price
        expected_total = 7.49 + (2 * 1.79)
        assert receipt.total_price() == pytest.approx(expected_total, 0.01)

    def test_multiple_offers_different_products(self):
        self.teller.add_special_offer(SpecialOfferType.THREE_FOR_TWO, self.toothbrush, 0)
        self.teller.add_special_offer(SpecialOfferType.TEN_PERCENT_DISCOUNT, self.apples, 20.0)

        cart = ShoppingCart()
        cart.add_item_quantity(self.toothbrush, 3)
        cart.add_item_quantity(self.apples, 2.0)

        receipt = self.teller.checks_out_articles_from(cart)

        # Calculate expected total
        toothbrush_total = 2 * 0.99  # 3 for 2 offer
        apple_original = 2.0 * 1.99
        apple_discount = apple_original * 0.20
        apple_total = apple_original - apple_discount

        expected_total = toothbrush_total + apple_total
        assert receipt.total_price() == pytest.approx(expected_total, 0.01)
        assert len(receipt.discounts) == 2
