from supermarket.catalog import SupermarketCatalog
from typing import Any, Optional

from supermarket.shopping_cart import ShoppingCart
from supermarket.product import ProductName, Product, ProductQuantity
from supermarket.offer import create_offer, Offer, SpecialOfferType
from supermarket.receipt import Receipt


class Teller:
    def __init__(self, catalog: SupermarketCatalog) -> None:
        self.catalog = catalog
        self.offers: dict[ProductName, Offer] = {}

    def add_special_offer(
        self,
        offer_type: SpecialOfferType,
        product: Product,
        argument: Optional[Any] = None,
    ) -> None:
        print(f"creating offer {offer_type}")
        self.offers[product.name] = create_offer(offer_type, product, argument)

    def checks_out_articles_from(self, cart: ShoppingCart) -> Receipt:
        receipt = Receipt()
        pq: ProductQuantity
        for pq in cart.items:
            product = pq.product
            quantity = pq.quantity
            unit_price: float = self.catalog.unit_price(product.name)
            price = quantity * unit_price
            receipt.add_product(product, quantity, unit_price, price)

        self.handle_offers(receipt, cart)

        return receipt

    def handle_offers(self, receipt: Receipt, cart: ShoppingCart):
        for product_name, quantity in cart.product_quantities.items():
            if product_name in self.offers.keys():
                offer = self.offers[product_name]
                unit_price = self.catalog.unit_price(product_name)
                discount = offer.calculate_discount(unit_price, quantity)
                if discount:
                    receipt.add_discount(discount)
