from enum import Enum
from .product import Product

class SpecialOfferType(Enum):
    THREE_FOR_TWO = 1
    TEN_PERCENT_DISCOUNT = 2
    TWO_FOR_AMOUNT = 3
    FIVE_FOR_AMOUNT = 4


class Offer:
    def __init__(self, offer_type: SpecialOfferType, product: Product, argument: int):
        self.offer_type = offer_type
        self.product = product
        self.argument = argument

    def __repr__(self):
        return (f"Offer(offer_type={self.offer_type.name}, "
                f"product={self.product}, argument={self.argument})")