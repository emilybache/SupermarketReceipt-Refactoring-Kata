import pytest
from approvaltests import verify

from model_objects import Product, SpecialOfferType, ProductUnit
from receipt_printer import ReceiptPrinter
from shopping_cart import ShoppingCart
from teller import Teller
from tests.fake_catalog import FakeCatalog


@pytest.fixture
def products():
    products = {
        "toothbrush": (Product("toothbrush", ProductUnit.EACH), 0.99),
        "apples": (Product("apples", ProductUnit.KILO), 1.99),
    }
    return products


@pytest.fixture
def cart():
    return ShoppingCart()


@pytest.fixture
def teller(catalog):
    return Teller(catalog)


@pytest.fixture
def catalog(products):
    catalog = FakeCatalog()
    for name, (product, price) in products.items():
        catalog.add_product(product, price)
    return catalog


def test_ten_percent_discount(teller, cart, products):
    toothbrush = products["toothbrush"][0]
    apples = products["apples"][0]

    teller.add_special_offer(SpecialOfferType.TEN_PERCENT_DISCOUNT, toothbrush, 10.0)
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


