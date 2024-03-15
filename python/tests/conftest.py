import pytest

from fake_catalog import FakeCatalog
from model_objects import Product, ProductUnit
from shopping_cart import ShoppingCart


@pytest.fixture
def shopping_cart():
    return ShoppingCart()


@pytest.fixture
def apple_product():
    return Product("apples", ProductUnit.KILO)


@pytest.fixture
def toothbrush_product():
    return Product("toothbrush", ProductUnit.EACH)


@pytest.fixture
def catalog():
    return FakeCatalog()


@pytest.fixture
def catalog_with_toothbrush(toothbrush_product, catalog):
    catalog.add_product(toothbrush_product, 0.99)
    return catalog


@pytest.fixture
def catalog_with_apple(apple_product, catalog):
    catalog.add_product(apple_product, 1.99)
    return catalog


@pytest.fixture
def catalog_with_toothbrush_and_apple(toothbrush_product, catalog_with_apple):
    catalog_with_apple.add_product(toothbrush_product, 0.99)
    return catalog_with_apple
