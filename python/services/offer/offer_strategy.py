from abc import ABC, abstractmethod
from typing import Optional

from models.offer import Offer
from models.discount import Discount
from models.product import Product

class OfferStrategy(ABC):
    @abstractmethod
    def calculate_discount(self, product: Product, quantity: int, unit_price: int, offer: Offer) -> Optional[Discount]:
        pass

class ThreeForTwoOffer(OfferStrategy):
    def calculate_discount(self, product: Product, quantity: int, unit_price: int, offer: Offer) -> Optional[Discount]:
        if quantity < 3:
            return None
        number_of_trios = quantity // 3
        discount_amount = number_of_trios * unit_price
        return Discount(product, "3 for 2", -discount_amount)

class TwoForAmountOffer(OfferStrategy):
    def calculate_discount(self, product: Product, quantity: int, unit_price: int, offer: Offer) -> Optional[Discount]:
        if quantity < 2:
            return None
        total_without_offer = quantity * unit_price
        total_with_offer = (quantity // 2) * offer.argument + (quantity % 2) * unit_price
        discount_amount = total_without_offer - total_with_offer
        return Discount(product, f"2 for {offer.argument}", -discount_amount)
    
class FiveForAmountOffer(OfferStrategy):
    def calculate_discount(self, product: Product, quantity: int, unit_price: int, offer: Offer) -> Optional[Discount]:
        if quantity < 5:
            return None
        total_without_offer = quantity * unit_price
        total_with_offer = (quantity // 5) * offer.argument + (quantity % 5) * unit_price
        discount_amount = total_without_offer - total_with_offer
        return Discount(product, f"5 for {offer.argument}", -discount_amount)

class TenPercentOffer(OfferStrategy):
    def calculate_discount(self, product: Product, quantity: int, unit_price: int, offer: Offer) -> Optional[Discount]:
        discount_amount = quantity * unit_price * offer.argument // 100
        return Discount(product, f"{offer.argument}% off", -discount_amount)