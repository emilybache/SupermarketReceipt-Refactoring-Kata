import pytest
from models.product import Product, ProductUnit
from models.discount import Discount
from models.offer import Offer, SpecialOfferType
from services.offer.offer_strategy import (
    ThreeForTwoOffer,
    TwoForAmountOffer,
    FiveForAmountOffer,
    TenPercentOffer
)

# Create some reusable product and offer objects for the tests
apple = Product("Apple", ProductUnit.EACH)
banana = Product("Banana", ProductUnit.EACH)

def test_three_for_two_offer():
    offer = Offer(SpecialOfferType.THREE_FOR_TWO, apple, 0)
    strategy = ThreeForTwoOffer()

    # Case 1: Buy 3 items, get 1 free
    discount = strategy.calculate_discount(apple, 3, 100, offer)
    assert discount is not None
    assert discount.discount_amount == -100
    assert discount.description == "3 for 2"

    # Case 2: Buy 4 items, get 1 free (only 3 counted for offer)
    discount = strategy.calculate_discount(apple, 4, 100, offer)
    assert discount is not None
    assert discount.discount_amount == -100
    assert discount.description == "3 for 2"

    # Case 3: Buy less than 3 items, no discount
    discount = strategy.calculate_discount(apple, 2, 100, offer)
    assert discount is None

def test_two_for_amount_offer():
    offer = Offer(SpecialOfferType.TWO_FOR_AMOUNT, banana, 150)
    strategy = TwoForAmountOffer()

    # Case 1: Buy 2 items, get a fixed price (150 for 2 instead of 200)
    discount = strategy.calculate_discount(banana, 2, 100, offer)
    assert discount is not None
    assert discount.discount_amount == -50  # 200 (100x2) - 150
    assert discount.description == "2 for 150"

    # Case 2: Buy 3 items, get a discount only for the first 2
    discount = strategy.calculate_discount(banana, 3, 100, offer)
    assert discount is not None
    assert discount.discount_amount == -50  # First 2 get the offer, 3rd is full price

    # Case 3: Buy 1 item, no discount
    discount = strategy.calculate_discount(banana, 1, 100, offer)
    assert discount is None

def test_five_for_amount_offer():
    offer = Offer(SpecialOfferType.FIVE_FOR_AMOUNT, apple, 400)
    strategy = FiveForAmountOffer()

    # Case 1: Buy exactly 5 items
    discount = strategy.calculate_discount(apple, 5, 100, offer)
    assert discount is not None
    assert discount.discount_amount == -100  # 500 (100x5) - 400
    assert discount.description == "5 for 400"

    # Case 2: Buy 6 items, get discount only for first 5
    discount = strategy.calculate_discount(apple, 6, 100, offer)
    assert discount is not None
    assert discount.discount_amount == -100  # First 5 get the offer, 6th is full price

    # Case 3: Buy less than 5 items, no discount
    discount = strategy.calculate_discount(apple, 3, 100, offer)
    assert discount is None

def test_ten_percent_offer():
    offer = Offer(SpecialOfferType.TEN_PERCENT_DISCOUNT, banana, 10)
    strategy = TenPercentOffer()

    # Case 1: Buy 1 item, get 10% off
    discount = strategy.calculate_discount(banana, 1, 100, offer)
    assert discount is not None
    assert discount.discount_amount == -10  # 10% of 100
    assert discount.description == "10% off"

    # Case 2: Buy 2 items, get 10% off each
    discount = strategy.calculate_discount(banana, 2, 100, offer)
    assert discount is not None
    assert discount.discount_amount == -20  # 10% of 200

    # Case 3: No discount for 0% off (edge case)
    no_discount_offer = Offer(SpecialOfferType.TEN_PERCENT_DISCOUNT, banana, 0)
    discount = strategy.calculate_discount(banana, 1, 100, no_discount_offer)
    assert discount is not None
    assert discount.discount_amount == 0  # No discount
