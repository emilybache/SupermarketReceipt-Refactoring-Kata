import approvaltests

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

