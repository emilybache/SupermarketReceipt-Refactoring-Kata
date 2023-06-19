import approvaltests
import pytest

from model_objects import SpecialOfferType
from receipt_printer import ReceiptPrinter


def test_empty_basket(teller, cart, toothbrush, apples):
    receipt = teller.checks_out_articles_from(cart)

    approvaltests.verify(ReceiptPrinter().print_receipt(receipt))

def test_no_discount(teller, cart, toothbrush, apples):
    teller.add_special_offer(SpecialOfferType.TEN_PERCENT_DISCOUNT, toothbrush, 10.0)
    cart.add_item_quantity(apples, 2.5)

    receipt = teller.checks_out_articles_from(cart)

    approvaltests.verify(ReceiptPrinter().print_receipt(receipt))


def test_ten_percent_discount(teller, cart, toothbrush):
    teller.add_special_offer(SpecialOfferType.TEN_PERCENT_DISCOUNT, toothbrush, 10.0)
    cart.add_item_quantity(toothbrush, 2)

    receipt = teller.checks_out_articles_from(cart)

    approvaltests.verify(ReceiptPrinter().print_receipt(receipt))


def test_three_for_two_discount(teller, cart, toothbrush):
    teller.add_special_offer(SpecialOfferType.THREE_FOR_TWO, toothbrush, 10.0)
    cart.add_item_quantity(toothbrush, 3)

    receipt = teller.checks_out_articles_from(cart)

    approvaltests.verify(ReceiptPrinter().print_receipt(receipt))

def test_three_for_two_discount_too_few(teller, cart, toothbrush):
    teller.add_special_offer(SpecialOfferType.THREE_FOR_TWO, toothbrush, 10.0)
    cart.add_item_quantity(toothbrush, 2)

    receipt = teller.checks_out_articles_from(cart)

    approvaltests.verify(ReceiptPrinter().print_receipt(receipt))


def test_five_for_amount_discount(teller, cart, toothbrush):
    teller.add_special_offer(SpecialOfferType.FIVE_FOR_AMOUNT, toothbrush, 4.0)
    cart.add_item_quantity(toothbrush, 5)

    receipt = teller.checks_out_articles_from(cart)

    approvaltests.verify(ReceiptPrinter().print_receipt(receipt))

def test_five_for_amount_discount_bought_too_few(teller, cart, toothbrush):
    teller.add_special_offer(SpecialOfferType.FIVE_FOR_AMOUNT, toothbrush, 4.0)
    cart.add_item_quantity(toothbrush, 4)

    receipt = teller.checks_out_articles_from(cart)

    approvaltests.verify(ReceiptPrinter().print_receipt(receipt))

def test_two_for_amount_discount(teller, cart, toothbrush):
    teller.add_special_offer(SpecialOfferType.TWO_FOR_AMOUNT, toothbrush, 1.80)
    cart.add_item_quantity(toothbrush, 5)

    receipt = teller.checks_out_articles_from(cart)

    approvaltests.verify(ReceiptPrinter().print_receipt(receipt))

def test_two_for_amount_discount_bought_too_few(teller, cart, toothbrush):
    teller.add_special_offer(SpecialOfferType.TWO_FOR_AMOUNT, toothbrush, 1.80)
    cart.add_item_quantity(toothbrush, 1)

    receipt = teller.checks_out_articles_from(cart)

    approvaltests.verify(ReceiptPrinter().print_receipt(receipt))
