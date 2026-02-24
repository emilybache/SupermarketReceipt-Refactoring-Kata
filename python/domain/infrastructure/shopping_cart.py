from ..entities.product import ProductQuantity


class ShoppingCart:

    def __init__(self):
        self._items = []
        self._product_quantities = {}

    @property
    def items(self):
        return self._items

    def add_item(self, product):
        self.add_item_quantity(product, 1.0)

    @property
    def product_quantities(self):
        return self._product_quantities

    def add_item_quantity(self, product, quantity):
        """Add a product with specified quantity to the cart"""
        self._items.append(ProductQuantity(product, quantity))
        self._product_quantities[product] = self._product_quantities.get(product, 0) + quantity

    def handle_offers(self, receipt, offers, bundle_offers, catalog):
        """Apply offers to products in the cart"""
        for product, quantity in self._product_quantities.items():
            if product in offers:
                offer = offers[product]
                unit_price = catalog.unit_price(product)
                discount = offer.calculate_discount(quantity, unit_price)
                
                if discount:
                    receipt.add_discount(discount)
        
        # Handle bundle offers
        for bundle in bundle_offers:
            discount = bundle.calculate_discount(self._product_quantities, catalog)
            if discount:
                receipt.add_discount(discount)
