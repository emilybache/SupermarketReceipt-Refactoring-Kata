import pytest

from python.domain.model_objects import (
    Product,
    ProductUnit,
    SpecialOfferType
)
from python.shopping_cart import ShoppingCart
from python.teller import Teller
from python.tests.fake_catalog import FakeCatalog


class TestEdgeCases:
    """Test edge cases and error conditions"""

    def setup_method(self):
        self.catalog = FakeCatalog()
        self.teller = Teller(self.catalog)

        self.toothbrush = Product("toothbrush", ProductUnit.EACH)
        self.catalog.add_product(self.toothbrush, 0.99)

    def test_zero_quantity_item(self):
        cart = ShoppingCart()
        cart.add_item_quantity(self.toothbrush, 0)

        receipt = self.teller.checks_out_articles_from(cart)

        assert receipt.total_price() == 0
        assert len(receipt.items) == 1
        assert receipt.items[0].quantity == 0

    def test_fractional_quantity_with_each_unit(self):
        # This might be problematic - fractional quantities for EACH units
        cart = ShoppingCart()
        cart.add_item_quantity(self.toothbrush, 2.5)

        receipt = self.teller.checks_out_articles_from(cart)

        # This should probably be handled better in a real system
        assert receipt.items[0].quantity == 2.5

    def test_very_large_quantities(self):
        cart = ShoppingCart()
        cart.add_item_quantity(self.toothbrush, 1000)

        receipt = self.teller.checks_out_articles_from(cart)

        expected_total = 1000 * 0.99
        assert receipt.total_price() == pytest.approx(expected_total, 0.01)


def test_ten_percent_discount():
    catalog = FakeCatalog()
    toothbrush = Product("toothbrush", ProductUnit.EACH)
    catalog.add_product(toothbrush, 0.99)

    apples = Product("apples", ProductUnit.KILO)
    catalog.add_product(apples, 1.99)

    teller = Teller(catalog)
    teller.add_special_offer(SpecialOfferType.TEN_PERCENT_DISCOUNT, toothbrush, 10.0)

    cart = ShoppingCart()
    cart.add_item_quantity(apples, 2.5)

    receipt = teller.checks_out_articles_from(cart)

    assert 4.975 == pytest.approx(receipt.total_price(), 0.01)
    assert [] == receipt.discounts
    assert 1 == len(receipt.items)
    receipt_item = receipt.items[0]
    assert apples == receipt_item.product
    assert 1.99 == receipt_item.price
    assert 2.5 * 1.99 == pytest.approx(receipt_item.total_price, 0.01)
    assert 2.5 == receipt_item.quantity
