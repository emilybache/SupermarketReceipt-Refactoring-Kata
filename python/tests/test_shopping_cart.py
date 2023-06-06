from supermarket.product import Product, ProductUnit
from supermarket.shopping_cart import ShoppingCart


def test_adding_items():
    shopping_cart = ShoppingCart()
    eggs = Product("eggs", ProductUnit.EACH)

    shopping_cart.add_item(eggs)

    assert shopping_cart.product_quantities[eggs.name] == 1.0

    shopping_cart.add_item_quantity(eggs, 2.0)

    assert shopping_cart.product_quantities[eggs.name] == 3.0
