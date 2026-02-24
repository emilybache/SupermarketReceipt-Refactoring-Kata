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
        self.offers = []  # Single list for all offers

    def add_special_offer(self, offer_type, product, argument):
        """Create appropriate offer instance based on offer type"""
        offer_map = {
            SpecialOfferType.THREE_FOR_TWO: lambda: ThreeForTwoOffer(product),
            SpecialOfferType.TWO_FOR_AMOUNT: lambda: TwoForAmountOffer(product, argument),
            SpecialOfferType.FIVE_FOR_AMOUNT: lambda: FiveForAmountOffer(product, argument),
            SpecialOfferType.TEN_PERCENT_DISCOUNT: lambda: TenPercentDiscountOffer(product, argument)
        }
        
        offer_factory = offer_map.get(offer_type)
        if offer_factory:
            self.offers.append(offer_factory())
    
    def add_offer(self, offer):
        """Add any offer polymorphically - works for all offer types including BundleOffer"""
        self.offers.append(offer)

    def checks_out_articles_from(self, the_cart):
        receipt = Receipt()
        product_quantities = the_cart.items
        for pq in product_quantities:
            p = pq.product
            quantity = pq.quantity
            unit_price = self.catalog.unit_price(p)
            price = quantity * unit_price
            receipt.add_product(p, quantity, unit_price, price)

        the_cart.handle_offers(receipt, self.offers, self.catalog)

        return receipt
