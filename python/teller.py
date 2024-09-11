from models.offer import Offer
from models.receipt import Receipt
from catalog import SupermarketCatalog as Catalog
from shopping_cart import ShoppingCart
from models.offer import SpecialOfferType
from models.product import Product

class Teller:

    def __init__(self, catalog: Catalog) -> None:
        self.catalog = catalog
        self.offers = {}

    # TODO:// maybe this one needed to be handled by some kind of manager
    def add_special_offer(self, offer_type: SpecialOfferType, product: Product, argument: int) -> None:
        self.offers[product] = Offer(offer_type, product, argument)
    # and teller can check out items only
    def checks_out_articles_from(self, the_cart: ShoppingCart) -> Receipt:
        receipt = Receipt()
        product_quantities = the_cart.items
        for pq in product_quantities:
            proudct = pq.product
            quantity = pq.quantity
            unit_price = self.catalog.unit_price(proudct)
            price = quantity * unit_price
            receipt.add_product(proudct, quantity, unit_price, price)

        the_cart.handle_offers(receipt, self.offers, self.catalog)

        return receipt
