import pytest

from python.domain.model_objects import Product, ProductUnit, SpecialOfferType
from python.shopping_cart import ShoppingCart
from python.teller import Teller
from python.tests.fake_catalog import FakeCatalog


class TestSupermarketBasicFunctionality:
    """Test basic supermarket functionality without discounts"""

    def setup_method(self):
        self.catalog = FakeCatalog()
        self.teller = Teller(self.catalog)

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

    def test_empty_cart(self):
        cart = ShoppingCart()
        receipt = self.teller.checks_out_articles_from(cart)

        assert receipt.total_price() == 0
        assert len(receipt.items) == 0
        assert len(receipt.discounts) == 0

    def test_single_item_each_unit(self):
        cart = ShoppingCart()
        cart.add_item(self.toothbrush)
        receipt = self.teller.checks_out_articles_from(cart)

        assert receipt.total_price() == 0.99
        assert len(receipt.items) == 1
        assert receipt.items[0].product == self.toothbrush
        assert receipt.items[0].quantity == 1.0
        assert receipt.items[0].price == 0.99
        assert receipt.items[0].total_price == 0.99

    def test_single_item_kilo_unit(self):
        cart = ShoppingCart()
        cart.add_item_quantity(self.apples, 2.5)
        receipt = self.teller.checks_out_articles_from(cart)

        assert receipt.total_price() == pytest.approx(4.975, 0.01)
        assert len(receipt.items) == 1
        assert receipt.items[0].product == self.apples
        assert receipt.items[0].quantity == 2.5
        assert receipt.items[0].price == 1.99

    def test_multiple_different_items(self):
        cart = ShoppingCart()
        cart.add_item(self.toothbrush)
        cart.add_item_quantity(self.apples, 1.5)
        cart.add_item(self.toothpaste)

        receipt = self.teller.checks_out_articles_from(cart)

        expected_total = 0.99 + (1.5 * 1.99) + 1.79
        assert receipt.total_price() == pytest.approx(expected_total, 0.01)
        assert len(receipt.items) == 3

    def test_multiple_same_items(self):
        cart = ShoppingCart()
        cart.add_item(self.toothbrush)
        cart.add_item(self.toothbrush)
        cart.add_item(self.toothbrush)

        receipt = self.teller.checks_out_articles_from(cart)

        # Should have 3 separate line items
        assert len(receipt.items) == 3
        assert receipt.total_price() == pytest.approx(2.97, 0.01)
