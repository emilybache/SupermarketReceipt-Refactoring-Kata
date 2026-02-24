from ..entities import (
    SpecialOfferType,
    ThreeForTwoOffer,
    TwoForAmountOffer,
    FiveForAmountOffer,
    TenPercentDiscountOffer,
    BundleOffer,
    Receipt
)


class Teller:

    def __init__(self, catalog):
        self.catalog = catalog
        self.offers = {}
        self.bundle_offers = []

    def add_special_offer(self, offer_type, product, argument):
        """Create appropriate offer instance based on offer type"""
        offer_map = {
            SpecialOfferType.THREE_FOR_TWO: ThreeForTwoOffer,
            SpecialOfferType.TWO_FOR_AMOUNT: TwoForAmountOffer,
            SpecialOfferType.FIVE_FOR_AMOUNT: FiveForAmountOffer,
            SpecialOfferType.TEN_PERCENT_DISCOUNT: TenPercentDiscountOffer
        }
        
        offer_class = offer_map.get(offer_type)
        if offer_class:
            self.offers[product] = offer_class(product, argument)
    
    def add_offer(self, offer):
        """Add any offer polymorphically"""
        if isinstance(offer, BundleOffer):
            self.bundle_offers.append(offer)
        else:
            # Regular offers are keyed by product
            self.offers[offer.product] = offer

    def checks_out_articles_from(self, the_cart):
        receipt = Receipt()
        product_quantities = the_cart.items
        for pq in product_quantities:
            p = pq.product
            quantity = pq.quantity
            unit_price = self.catalog.unit_price(p)
            price = quantity * unit_price
            receipt.add_product(p, quantity, unit_price, price)

        the_cart.handle_offers(receipt, self.offers, self.bundle_offers, self.catalog)

        return receipt
