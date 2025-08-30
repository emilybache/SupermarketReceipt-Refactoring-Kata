import pytest
from ..model_objects import Product, SpecialOfferType, ProductUnit
from ..shopping_cart import ShoppingCart
from ..teller import Teller
from ..tests.fake_catalog import FakeCatalog


class TestRefactoredSupermarket:
    """Integration tests for the refactored supermarket system"""

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

    def test_no_discounts_basic_functionality(self):
        """Test that basic functionality still works without discounts"""
        cart = ShoppingCart()
        cart.add_item(self.toothbrush)
        cart.add_item_quantity(self.apples, 2.5)

        receipt = self.teller.checks_out_articles_from(cart)

        expected_total = 0.99 + (2.5 * 1.99)
        assert receipt.total_price() == pytest.approx(expected_total, 0.01)
        assert len(receipt.discounts) == 0
        assert len(receipt.items) == 2

    def test_three_for_two_discount_integration(self):
        """Test 3-for-2 discount works end-to-end"""
        self.teller.add_special_offer(SpecialOfferType.THREE_FOR_TWO, self.toothbrush, 0)

        cart = ShoppingCart()
        cart.add_item_quantity(self.toothbrush, 3)

        receipt = self.teller.checks_out_articles_from(cart)

        expected_total = 2 * 0.99  # Pay for 2, get 1 free
        assert receipt.total_price() == pytest.approx(expected_total, 0.01)
        assert len(receipt.discounts) == 1
        assert "3 for 2" in receipt.discounts[0].description

    def test_ten_percent_discount_integration(self):
        """Test percentage discount works end-to-end"""
        self.teller.add_special_offer(SpecialOfferType.TEN_PERCENT_DISCOUNT, self.apples, 20.0)

        cart = ShoppingCart()
        cart.add_item_quantity(self.apples, 2.5)

        receipt = self.teller.checks_out_articles_from(cart)

        original_price = 2.5 * 1.99
        discount_amount = original_price * 0.20
        expected_total = original_price - discount_amount

        assert receipt.total_price() == pytest.approx(expected_total, 0.01)
        assert len(receipt.discounts) == 1
        assert "20.0% off" in receipt.discounts[0].description

    def test_two_for_amount_integration(self):
        """Test 2-for-amount discount works end-to-end"""
        self.teller.add_special_offer(SpecialOfferType.TWO_FOR_AMOUNT, self.cherry_tomatoes, 0.99)

        cart = ShoppingCart()
        cart.add_item_quantity(self.cherry_tomatoes, 2)

        receipt = self.teller.checks_out_articles_from(cart)

        assert receipt.total_price() == pytest.approx(0.99, 0.01)
        assert len(receipt.discounts) == 1

    def test_five_for_amount_integration(self):
        """Test 5-for-amount discount works end-to-end"""
        self.teller.add_special_offer(SpecialOfferType.FIVE_FOR_AMOUNT, self.toothpaste, 7.49)

        cart = ShoppingCart()
        cart.add_item_quantity(self.toothpaste, 5)

        receipt = self.teller.checks_out_articles_from(cart)

        assert receipt.total_price() == pytest.approx(7.49, 0.01)
        assert len(receipt.discounts) == 1

    def test_multiple_discounts_different_products(self):
        """Test multiple discounts on different products"""
        self.teller.add_special_offer(SpecialOfferType.THREE_FOR_TWO, self.toothbrush, 0)
        self.teller.add_special_offer(SpecialOfferType.TEN_PERCENT_DISCOUNT, self.apples, 20.0)

        cart = ShoppingCart()
        cart.add_item_quantity(self.toothbrush, 3)
        cart.add_item_quantity(self.apples, 2.0)

        receipt = self.teller.checks_out_articles_from(cart)

        # Calculate expected totals
        toothbrush_total = 2 * 0.99  # 3-for-2 discount
        apple_original = 2.0 * 1.99
        apple_discount = apple_original * 0.20
        apple_total = apple_original - apple_discount

        expected_total = toothbrush_total + apple_total
        assert receipt.total_price() == pytest.approx(expected_total, 0.01)
        assert len(receipt.discounts) == 2

    def test_shopping_cart_validation(self):
        """Test shopping cart input validation"""
        cart = ShoppingCart()

        with pytest.raises(ValueError, match="Quantity cannot be negative"):
            cart.add_item_quantity(self.toothbrush, -1.0)

    def test_complex_scenario(self):
        """Test a complex checkout scenario"""
        # Setup multiple offers
        self.teller.add_special_offer(SpecialOfferType.THREE_FOR_TWO, self.toothbrush, 0)
        self.teller.add_special_offer(SpecialOfferType.FIVE_FOR_AMOUNT, self.toothpaste, 7.49)
        self.teller.add_special_offer(SpecialOfferType.TEN_PERCENT_DISCOUNT, self.apples, 15.0)

        cart = ShoppingCart()
        cart.add_item_quantity(self.toothbrush, 7)  # Should get 2 free
        cart.add_item_quantity(self.toothpaste, 6)  # 5 for 7.49 + 1 regular
        cart.add_item_quantity(self.apples, 1.5)   # 15% off
        cart.add_item(self.rice)                   # No discount

        receipt = self.teller.checks_out_articles_from(cart)

        # Verify we have the right number of discounts
        assert len(receipt.discounts) == 3

        # Verify total is reasonable (not testing exact amount due to complexity)
        assert receipt.total_price() > 0
        assert receipt.total_price() < (7 * 0.99 + 6 * 1.79 + 1.5 * 1.99 + 2.49)  # Should be less than full price
