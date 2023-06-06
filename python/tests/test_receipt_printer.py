import pytest

from supermarket.product import Product, ProductUnit
from supermarket.receipt import Receipt, Discount
from supermarket.receipt_printer import ReceiptPrinter


@pytest.fixture
def receipt():
    receipt = Receipt()
    receipt.add_product(Product("apples", ProductUnit.KILO), 3, 2.99, 2.99 * 3)
    receipt.add_product(Product("eggs", ProductUnit.EACH), 10, 0.10, 0.10 * 10)
    return receipt


def test_print_receipt_default(receipt):
    receipt_printer = ReceiptPrinter()

    result = receipt_printer.print_receipt(receipt)

    assert result == (
        "apples                              8.97\n"
        "  2.99 * 3.000\n"
        "eggs                                1.00\n"
        "  0.10 * 10\n"
        "\n"
        "Total:                              9.97\n"
    )


def test_print_receipt_20_cols(receipt):
    receipt_printer = ReceiptPrinter(columns=20)

    result = receipt_printer.print_receipt(receipt)

    assert result == (
        "apples          8.97\n"
        "  2.99 * 3.000\n"
        "eggs            1.00\n"
        "  0.10 * 10\n"
        "\n"
        "Total:          9.97\n"
    )


def test_print_receipt_with_discount(receipt):
    receipt_printer = ReceiptPrinter()
    receipt.add_discount(Discount("eggs", "10% off", -0.10))

    result = receipt_printer.print_receipt(receipt)

    assert result == (
        "apples                              8.97\n"
        "  2.99 * 3.000\n"
        "eggs                                1.00\n"
        "  0.10 * 10\n"
        "10% off (eggs)                     -0.10\n"
        "\n"
        "Total:                              9.87\n"
    )
