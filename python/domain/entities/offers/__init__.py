"""Offer classes for special discounts"""
from .offer import Offer
from .three_for_two_offer import ThreeForTwoOffer
from .two_for_amount_offer import TwoForAmountOffer
from .five_for_amount_offer import FiveForAmountOffer
from .ten_percent_discount_offer import TenPercentDiscountOffer
from .bundle_offer import BundleOffer

__all__ = [
    'Offer',
    'ThreeForTwoOffer',
    'TwoForAmountOffer',
    'FiveForAmountOffer',
    'TenPercentDiscountOffer',
    'BundleOffer'
]
