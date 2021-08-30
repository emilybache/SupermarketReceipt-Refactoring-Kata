"""
Start texttest from a command prompt in the same folder as this file with this command:

texttest -a sr -d .
"""

import sys,csv
from pathlib import Path

from model_objects import Product, SpecialOfferType, ProductUnit
from receipt_printer import ReceiptPrinter
from shopping_cart import ShoppingCart
from teller import Teller
from tests.fake_catalog import FakeCatalog


def read_catalog(catalog_file):
    catalog = FakeCatalog()
    if not catalog_file.exists():
        return catalog
    with open(catalog_file, "r") as f:
        reader = csv.DictReader(f)
        for row in reader:
            name = row['name']
            unit = ProductUnit[row['unit']]
            price = float(row['price'])
            product = Product(name, unit)
            catalog.add_product(product, price)
    return catalog


def read_offers(offers_file, teller):
    if not offers_file.exists():
        return
    with open(offers_file, "r") as f:
        reader = csv.DictReader(f)
        for row in reader:
            name = row['name']
            offerType = SpecialOfferType[row['offer']]
            argument = float(row['argument'])
            product = teller.product_with_name(name)
            teller.add_special_offer(offerType, product, argument)


def read_basket(cart_file, catalog):
    cart = ShoppingCart()
    if not cart_file.exists():
        return cart
    with open(cart_file, "r") as f:
        reader = csv.DictReader(f)
        for row in reader:
            name = row['name']
            quantity = float(row['quantity'])
            product = catalog.products[name]
            cart.add_item_quantity(product, quantity)
    return cart


def main(args):
    catalog = read_catalog(Path("catalog.csv"))
    teller = Teller(catalog)
    read_offers(Path("offers.csv"), teller)
    basket = read_basket(Path("cart.csv"), catalog)
    receipt = teller.checks_out_articles_from(basket)
    print(ReceiptPrinter().print_receipt(receipt))


if __name__ == "__main__":
    main(sys.argv[1:])