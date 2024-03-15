import approvaltests
import pytest

from catalog import SupermarketCatalog
from model_objects import SpecialOfferType
from receipt_printer import ReceiptPrinter
from teller import Teller


def test_basic_functionality(catalog_with_toothbrush_and_apple, apple_product, shopping_cart):
    teller = Teller(catalog_with_toothbrush_and_apple)
    shopping_cart.add_item_quantity(apple_product, 2.5)

    receipt = teller.checks_out_articles_from(shopping_cart)

    string_receipt = ReceiptPrinter().print_receipt(receipt)

    approvaltests.verify(string_receipt)


def test_ten_percent_discount(catalog_with_toothbrush, toothbrush_product, shopping_cart):
    teller = Teller(catalog_with_toothbrush)
    teller.add_special_offer(SpecialOfferType.TEN_PERCENT_DISCOUNT, toothbrush_product, 10.0)

    shopping_cart.add_item_quantity(toothbrush_product, 2.5)
    receipt = teller.checks_out_articles_from(shopping_cart)
    string_receipt = ReceiptPrinter().print_receipt(receipt)

    approvaltests.verify(string_receipt)


def test_three_for_two_discount(catalog_with_toothbrush, toothbrush_product, shopping_cart):
    teller = Teller(catalog_with_toothbrush)
    teller.add_special_offer(SpecialOfferType.THREE_FOR_TWO, toothbrush_product, 2)

    shopping_cart.add_item_quantity(toothbrush_product, 3)
    receipt = teller.checks_out_articles_from(shopping_cart)
    string_receipt = ReceiptPrinter().print_receipt(receipt)

    approvaltests.verify(string_receipt)


def test_two_items_for_reduced_price_discount(catalog_with_toothbrush, toothbrush_product, shopping_cart):
    teller = Teller(catalog_with_toothbrush)
    teller.add_special_offer(SpecialOfferType.TWO_FOR_AMOUNT, toothbrush_product, 0.99)

    shopping_cart.add_item_quantity(toothbrush_product, 2)
    receipt = teller.checks_out_articles_from(shopping_cart)
    string_receipt = ReceiptPrinter().print_receipt(receipt)

    approvaltests.verify(string_receipt)


def test_five_items_for_reduced_price_discount(catalog_with_toothbrush, toothbrush_product, shopping_cart):
    teller = Teller(catalog_with_toothbrush)
    teller.add_special_offer(SpecialOfferType.FIVE_FOR_AMOUNT, toothbrush_product, 4)

    shopping_cart.add_item_quantity(toothbrush_product, 9)
    receipt = teller.checks_out_articles_from(shopping_cart)
    string_receipt = ReceiptPrinter().print_receipt(receipt)

    approvaltests.verify(string_receipt)


def test_cart_with_same_product_no_discount(catalog_with_toothbrush, toothbrush_product, shopping_cart):
    teller = Teller(catalog_with_toothbrush)
    teller.add_special_offer(SpecialOfferType.FIVE_FOR_AMOUNT, toothbrush_product, 4)

    shopping_cart.add_item_quantity(toothbrush_product, 1)
    shopping_cart.add_item_quantity(toothbrush_product, 1)
    receipt = teller.checks_out_articles_from(shopping_cart)
    string_receipt = ReceiptPrinter().print_receipt(receipt)

    approvaltests.verify(string_receipt)


def test_add_product_exception_on_catalog(apple_product):
    catalog = SupermarketCatalog()

    with pytest.raises(Exception) as ex:
        catalog.add_product(apple_product, 10)

    assert str(ex.value) == 'cannot be called from a unit test - it accesses the database'


def test_unit_price_exception_on_catalog(apple_product):
    catalog = SupermarketCatalog()

    with pytest.raises(Exception) as ex:
        catalog.unit_price(apple_product)

    assert str(ex.value) == 'cannot be called from a unit test - it accesses the database'
