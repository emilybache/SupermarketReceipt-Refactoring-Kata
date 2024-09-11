import math
from typing import List, Dict
from collections import defaultdict

from models.product import ProductQuantity, Product
from models.receipt import Receipt
from models.offer import Offer

from catalog import SupermarketCatalog as Catalog

from services.offer.offer_service import OfferService

class ShoppingCart:
    """should be focused on managing items and quantities"""
    def __init__(self) -> None:
        self._items: List[ProductQuantity] = []
        self._product_quantities: defaultdict = defaultdict(int)
        self.offer_service = OfferService()

    @property
    def items(self) -> List[ProductQuantity]:
        return self._items
    
    @property
    def product_quantities(self) -> defaultdict:
        return self._product_quantities

    def add_item(self, product: Product) -> None:
        self.add_item_quantity(product, 1)

    def add_item_quantity(self, product: Product, quantity: int) -> None:
        self._items.append(ProductQuantity(product, quantity))
        self._product_quantities[product] += quantity # defaultdict will add defulat 0 value

    # TODO:// we might need to move this logic to another class
    def handle_offers(self, receipt: Receipt, offers: Dict[Product, Offer], catalog: Catalog) -> None:
        for product, quantity in self._product_quantities.items():
            if product in offers:
                offer = offers[product]
                unit_price = catalog.unit_price(product)
                discount = self.offer_service.apply_offer(offer, product, quantity, unit_price)
                if discount:
                    receipt.add_discount(discount)
