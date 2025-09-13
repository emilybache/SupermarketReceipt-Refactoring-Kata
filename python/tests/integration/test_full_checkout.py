import pytest
from python.domain.model_objects import Product, SpecialOfferType, ProductUnit
from python.shopping_cart import ShoppingCart
from python.teller import Teller
from python.tests.fake_catalog import FakeCatalog
from python.infrastructure.receipt_printer import ReceiptPrinter


class TestFullCheckoutIntegration:
    """Integration tests for the complete checkout process"""

    def setup_method(self):
        self.catalog = FakeCatalog()
        self.teller = Teller(self.catalog)
        self.printer = ReceiptPrinter()

        # Setup common products
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

    def test_empty_cart_checkout(self):
        """Test checkout with empty cart"""
        cart = ShoppingCart()
        receipt = self.teller.checks_out_articles_from(cart)

        assert receipt.total_price() == 0
        assert len(receipt.items) == 0
        assert len(receipt.discounts) == 0

    def test_simple_checkout_no_discounts(self):
        """Test checkout without any discounts"""
        cart = ShoppingCart()
        cart.add_item(self.toothbrush)
        cart.add_item_quantity(self.apples, 2.5)

        receipt = self.teller.checks_out_articles_from(cart)

        expected_total = 0.99 + (2.5 * 1.99)
        assert receipt.total_price() == pytest.approx(expected_total, 0.01)
        assert len(receipt.discounts) == 0
        assert len(receipt.items) == 2

    def test_three_for_two_discount_integration(self):
        """Test 3-for-2 discount end-to-end"""
        self.teller.add_special_offer(SpecialOfferType.THREE_FOR_TWO, self.toothbrush, 0)

        cart = ShoppingCart()
        cart.add_item_quantity(self.toothbrush, 3)

        receipt = self.teller.checks_out_articles_from(cart)

        # Should pay for 2, get 1 free
        expected_total = 2 * 0.99
        assert receipt.total_price() == pytest.approx(expected_total, 0.01)
        assert len(receipt.discounts) == 1
        assert "3 for 2" in receipt.discounts[0].description

    def test_percentage_discount_integration(self):
        """Test percentage discount end-to-end"""
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
        """Test 2-for-amount discount end-to-end"""
        self.teller.add_special_offer(SpecialOfferType.TWO_FOR_AMOUNT, self.cherry_tomatoes, 0.99)

        cart = ShoppingCart()
        cart.add_item_quantity(self.cherry_tomatoes, 2)

        receipt = self.teller.checks_out_articles_from(cart)

        assert receipt.total_price() == pytest.approx(0.99, 0.01)
        assert len(receipt.discounts) == 1

    def test_five_for_amount_integration(self):
        """Test 5-for-amount discount end-to-end"""
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

    def test_complex_scenario_all_discount_types(self):
        """Test complex scenario with all discount types"""
        # Setup multiple offers
        self.teller.add_special_offer(SpecialOfferType.THREE_FOR_TWO, self.toothbrush, 0)
        self.teller.add_special_offer(SpecialOfferType.FIVE_FOR_AMOUNT, self.toothpaste, 7.49)
        self.teller.add_special_offer(SpecialOfferType.TEN_PERCENT_DISCOUNT, self.apples, 15.0)
        self.teller.add_special_offer(SpecialOfferType.TWO_FOR_AMOUNT, self.cherry_tomatoes, 0.99)

        cart = ShoppingCart()
        cart.add_item_quantity(self.toothbrush, 7)      # Should get 2 free
        cart.add_item_quantity(self.toothpaste, 6)     # 5 for 7.49 + 1 regular
        cart.add_item_quantity(self.apples, 1.5)       # 15% off
        cart.add_item_quantity(self.cherry_tomatoes, 4) # 2 pairs at discount
        cart.add_item(self.rice)                       # No discount

        receipt = self.teller.checks_out_articles_from(cart)

        # Verify we have the right number of discounts
        assert len(receipt.discounts) == 4

        # Verify total is reasonable and less than full price
        full_price = (7 * 0.99 + 6 * 1.79 + 1.5 * 1.99 + 4 * 0.69 + 2.49)
        assert receipt.total_price() > 0
        assert receipt.total_price() < full_price

    def test_receipt_printing_with_discounts(self):
        """Test that receipt printing works with discounts"""
        self.teller.add_special_offer(SpecialOfferType.THREE_FOR_TWO, self.toothbrush, 0)
        self.teller.add_special_offer(SpecialOfferType.TEN_PERCENT_DISCOUNT, self.apples, 10.0)

        cart = ShoppingCart()
        cart.add_item_quantity(self.toothbrush, 3)
        cart.add_item_quantity(self.apples, 2.0)

        receipt = self.teller.checks_out_articles_from(cart)
        printed_receipt = self.printer.print_receipt(receipt)

        # Check that all expected elements are in the printed receipt
        assert "toothbrush" in printed_receipt
        assert "apples" in printed_receipt
        assert "3 for 2" in printed_receipt
        assert "10.0% off" in printed_receipt
        assert "Total:" in printed_receipt

    def test_shopping_cart_validation(self):
        """Test shopping cart input validation"""
        cart = ShoppingCart()

        # Test negative quantity validation
        with pytest.raises(ValueError, match="Quantity cannot be negative"):
            cart.add_item_quantity(self.toothbrush, -1.0)

        # Test None product validation
        with pytest.raises(ValueError, match="Product cannot be None"):
            cart.add_item_quantity(None, 1.0)

    def test_offer_management(self):
        """Test adding and removing special offers"""
        # Initially no offers
        assert len(self.teller.get_special_offers()) == 0

        # Add an offer
        self.teller.add_special_offer(SpecialOfferType.THREE_FOR_TWO, self.toothbrush, 0)
        offers = self.teller.get_special_offers()
        assert len(offers) == 1
        assert self.toothbrush in offers

        # Remove the offer
        self.teller.remove_special_offer(self.toothbrush)
        assert len(self.teller.get_special_offers()) == 0

    def test_total_before_discounts(self):
        """Test calculating total before discounts"""
        self.teller.add_special_offer(SpecialOfferType.THREE_FOR_TWO, self.toothbrush, 0)

        cart = ShoppingCart()
        cart.add_item_quantity(self.toothbrush, 3)

        total_before = self.teller.calculate_total_before_discounts(cart)
        receipt = self.teller.checks_out_articles_from(cart)
        total_after = receipt.total_price()

        # Total before should be higher than total after (due to discount)
        assert total_before == pytest.approx(3 * 0.99, 0.01)
        assert total_after == pytest.approx(2 * 0.99, 0.01)
        assert total_before > total_after

    def test_cart_operations(self):
        """Test additional shopping cart operations"""
        cart = ShoppingCart()

        # Test empty cart
        assert cart.is_empty()
        assert cart.get_item_count() == 0
        assert len(cart.get_products()) == 0

        # Add items
        cart.add_item(self.toothbrush)
        cart.add_item_quantity(self.apples, 2.5)

        assert not cart.is_empty()
        assert cart.get_item_count() == 2
        assert len(cart.get_products()) == 2
        assert self.toothbrush in cart.get_products()
        assert self.apples in cart.get_products()

        # Remove items
        removed_count = cart.remove_item(self.toothbrush)
        assert removed_count == 1
        assert self.toothbrush not in cart.get_products()
        assert cart.get_item_count() == 1

        # Clear cart
        cart.clear()
        assert cart.is_empty()
        assert cart.get_item_count() == 0
