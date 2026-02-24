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

    def handle_offers(self, receipt, offers, catalog):
        """Apply all offers to products in the cart.
        
        All offers are now treated uniformly through polymorphism.
        Each offer knows how to calculate its own discount based on cart contents.
        """
        for offer in offers:
            discount = offer.calculate_discount(self, catalog)
            if discount:
                receipt.add_discount(discount)
