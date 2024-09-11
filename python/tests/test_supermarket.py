from models.product import Product, ProductUnit
from models.offer import SpecialOfferType
from shopping_cart import ShoppingCart
from teller import Teller
from tests.fake_catalog import FakeCatalog


def test_no_offers():
    catalog = FakeCatalog()

    apples = Product("apples", ProductUnit.KILO)
    catalog.add_product(apples, 199)

    teller = Teller(catalog)
    cart = ShoppingCart()
    # TODO:// we need to replace the KILO with GRAM
    # to handel the 2.5K without using float
    cart.add_item_quantity(apples, 2)

    receipt = teller.checks_out_articles_from(cart)

    assert 398 == receipt.total_price()
    assert [] == receipt.discounts
    assert 1 == len(receipt.items)
    receipt_item = receipt.items[0]
    assert apples == receipt_item.product
    assert 199 == receipt_item.price
    assert 2 * 199 == receipt_item.total_price
    assert 2 == receipt_item.quantity


def test_ten_percent_discount():
    catalog = FakeCatalog()
    toothbrush = Product("toothbrush", ProductUnit.EACH)
    catalog.add_product(toothbrush, 99)

    teller = Teller(catalog)
    teller.add_special_offer(SpecialOfferType.TEN_PERCENT_DISCOUNT, toothbrush, 10)

    cart = ShoppingCart()
    cart.add_item_quantity(toothbrush, 1)

    receipt = teller.checks_out_articles_from(cart)

    assert 90 == receipt.total_price()
    assert receipt.discounts == receipt.discounts
    assert 1 == len(receipt.items)
    receipt_item = receipt.items[0]
    assert toothbrush == receipt_item.product
    assert 99 == receipt_item.price
    assert 1 * 99 == receipt_item.total_price
    assert 1 == receipt_item.quantity
