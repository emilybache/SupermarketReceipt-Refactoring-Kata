from ..model_objects import (
    Product,
    SpecialOfferType,
    ProductUnit
)
from ..receipt_printer import ReceiptPrinter
from ..shopping_cart import ShoppingCart
from ..teller import Teller
from .fake_catalog import FakeCatalog


class TestReceiptPrinter:
    """Test receipt formatting and printing"""

    def setup_method(self):
        self.catalog = FakeCatalog()
        self.teller = Teller(self.catalog)
        self.printer = ReceiptPrinter()

        self.toothbrush = Product("toothbrush", ProductUnit.EACH)
        self.apples = Product("apples", ProductUnit.KILO)

        self.catalog.add_product(self.toothbrush, 0.99)
        self.catalog.add_product(self.apples, 1.99)

    def test_print_simple_receipt(self):
        cart = ShoppingCart()
        cart.add_item(self.toothbrush)

        receipt = self.teller.checks_out_articles_from(cart)
        printed_receipt = self.printer.print_receipt(receipt)

        assert "toothbrush" in printed_receipt
        assert "0.99" in printed_receipt
        assert "Total:" in printed_receipt

    def test_print_receipt_with_quantity(self):
        cart = ShoppingCart()
        cart.add_item_quantity(self.apples, 2.5)

        receipt = self.teller.checks_out_articles_from(cart)
        printed_receipt = self.printer.print_receipt(receipt)

        assert "apples" in printed_receipt
        assert "2.500" in printed_receipt  # Kilo quantities should show 3 decimal places
        assert "1.99" in printed_receipt  # Unit price

    def test_print_receipt_with_discount(self):
        self.teller.add_special_offer(SpecialOfferType.TEN_PERCENT_DISCOUNT, self.toothbrush, 10.0)

        cart = ShoppingCart()
        cart.add_item(self.toothbrush)

        receipt = self.teller.checks_out_articles_from(cart)
        printed_receipt = self.printer.print_receipt(receipt)

        assert "10.0% off (toothbrush)" in printed_receipt
