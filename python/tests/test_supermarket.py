import pytest
from dataclasses import dataclass
from typing import Any, Optional

from supermarket.product import Product, ProductName, ProductUnit
from supermarket.offer import SpecialOfferType
from supermarket.shopping_cart import ShoppingCart
from supermarket.teller import Teller

from tests.fake_catalog import FakeCatalog

APPLES_PRICE = 1.99
TOOTHBRUSH_PRICE = 0.99


@pytest.fixture
def catalog() -> FakeCatalog:
    _catalog = FakeCatalog()
    _catalog.add_product(Product("toothbrush", ProductUnit.EACH), TOOTHBRUSH_PRICE)
    _catalog.add_product(Product("apples", ProductUnit.KILO), APPLES_PRICE)
    return _catalog


@pytest.fixture
def teller(catalog) -> Teller:
    return Teller(catalog)


@dataclass
class OfferTestData:
    offer_type: SpecialOfferType
    product: ProductName
    argument: Optional[Any] = None


@dataclass
class ReceiptTestCase:
    name: str
    active_offers: list[OfferTestData]
    purchased_quantities: dict[ProductName, float]
    applicable_discounts: int
    expected_total: float


SPECIAL_OFFER_TEST_CASES = [
    ReceiptTestCase(
        name="no active offers",
        active_offers=[],
        purchased_quantities={"apples": 20},
        applicable_discounts=0,
        expected_total=20 * APPLES_PRICE,
    ),
    ReceiptTestCase(
        name="10% on toothbrush, but only apples bought",
        active_offers=[
            OfferTestData(
                offer_type=SpecialOfferType.PERCENTAGE_DISCOUNT,
                argument=10.0,
                product="toothbrush",
            ),
        ],
        purchased_quantities={"apples": 2.5},
        applicable_discounts=0,
        expected_total=2.5 * APPLES_PRICE,
    ),
    ReceiptTestCase(
        name="10% on toothbrush",
        active_offers=[
            OfferTestData(
                offer_type=SpecialOfferType.PERCENTAGE_DISCOUNT,
                argument=10.0,
                product="toothbrush",
            ),
        ],
        purchased_quantities={"toothbrush": 10},
        applicable_discounts=1,
        expected_total=10 * TOOTHBRUSH_PRICE * 0.9,
    ),
    ReceiptTestCase(
        name="10% on toothbrush and apples",
        active_offers=[
            OfferTestData(
                offer_type=SpecialOfferType.PERCENTAGE_DISCOUNT,
                argument=10.0,
                product="toothbrush",
            ),
            OfferTestData(
                offer_type=SpecialOfferType.PERCENTAGE_DISCOUNT,
                argument=10.0,
                product="apples",
            ),
        ],
        purchased_quantities={"toothbrush": 2, "apples": 7},
        applicable_discounts=2,
        expected_total=(2 * TOOTHBRUSH_PRICE + 7 * APPLES_PRICE) * 0.9,
    ),
    ReceiptTestCase(
        name="3 for 2, but only 2 bought",
        active_offers=[
            OfferTestData(
                offer_type=SpecialOfferType.QUANTITY_FOR_QUANTITY,
                product="toothbrush",
                argument=(3, 2)
            ),
        ],
        purchased_quantities={"toothbrush": 2},
        applicable_discounts=0,
        expected_total=2 * TOOTHBRUSH_PRICE,
    ),
    ReceiptTestCase(
        name="3 for 2, bought 3",
        active_offers=[
            OfferTestData(
                offer_type=SpecialOfferType.QUANTITY_FOR_QUANTITY,
                product="toothbrush",
                argument=(3, 2)
            ),
        ],
        purchased_quantities={"toothbrush": 3},
        applicable_discounts=1,
        expected_total=2 * TOOTHBRUSH_PRICE,
    ),
    ReceiptTestCase(
        name="3 for 2, bought 4",
        active_offers=[
            OfferTestData(
                offer_type=SpecialOfferType.QUANTITY_FOR_QUANTITY,
                product="toothbrush",
                argument=(3, 2)
            ),
        ],
        purchased_quantities={"toothbrush": 4},
        applicable_discounts=1,
        expected_total=3 * TOOTHBRUSH_PRICE,
    ),
    ReceiptTestCase(
        name="5 for amount: 5 toothbrushes for $3.14",
        active_offers=[
            OfferTestData(
                offer_type=SpecialOfferType.QUANTITY_FOR_AMOUNT,
                argument=(5, 3.14),
                product="toothbrush",
            ),
        ],
        purchased_quantities={"toothbrush": 5},
        applicable_discounts=1,
        expected_total=3.14,
    ),
    ReceiptTestCase(
        name="5 for amount: not applicable to bought items",
        active_offers=[
            OfferTestData(
                offer_type=SpecialOfferType.QUANTITY_FOR_AMOUNT,
                argument=(5, 3.14),
                product="toothbrush",
            ),
        ],
        purchased_quantities={"apples": 10},
        applicable_discounts=0,
        expected_total=10 * APPLES_PRICE,
    ),
    ReceiptTestCase(
        name="2 for amount: not enough bought",
        active_offers=[
            OfferTestData(
                offer_type=SpecialOfferType.QUANTITY_FOR_AMOUNT,
                argument=(2, 0.50),
                product="toothbrush",
            ),
        ],
        purchased_quantities={"toothbrush": 1},
        applicable_discounts=0,
        expected_total=TOOTHBRUSH_PRICE,
    ),
    ReceiptTestCase(
        name="2 for amount: 7 bought (effective 3 times + remainder)",
        active_offers=[
            OfferTestData(
                offer_type=SpecialOfferType.QUANTITY_FOR_AMOUNT,
                argument=(2, 0.50),
                product="toothbrush",
            ),
        ],
        purchased_quantities={"toothbrush": 7},
        applicable_discounts=1,
        expected_total=0.50 * 3 + TOOTHBRUSH_PRICE,
    ),
]


@pytest.mark.parametrize("test_case", SPECIAL_OFFER_TEST_CASES)
def test_receipt_with_special_offers(test_case: ReceiptTestCase, teller: Teller):
    catalog = teller.catalog
    for offer in test_case.active_offers:
        teller.add_special_offer(
            offer.offer_type, catalog.products[offer.product], offer.argument
        )

    cart = ShoppingCart()
    for product_name, quantity in test_case.purchased_quantities.items():
        product = catalog.products[product_name]
        cart.add_item_quantity(product, quantity)

    receipt = teller.checks_out_articles_from(cart)

    assert pytest.approx(receipt.total_price(), 0.01) == test_case.expected_total

    assert len(receipt.discounts) == test_case.applicable_discounts
    # TODO: verify discounts

    assert len(receipt.items) == len(test_case.purchased_quantities)

    for receipt_item in receipt.items:
        product_name = receipt_item.product.name
        purchased_quantity = test_case.purchased_quantities[product_name]
        assert receipt_item.quantity == purchased_quantity
        assert receipt_item.product == catalog.products[product_name]
        product_price = catalog.prices[product_name]
        assert receipt_item.price == product_price
        product_total = product_price * purchased_quantity
        assert pytest.approx(receipt_item.total_price, 0.01) == product_total
