from python.domain.model_objects import ProductQuantity


class ShoppingCart:
    """
    Represents a shopping cart containing items to be purchased.

    This class is responsible for:
    - Managing items in the cart
    - Providing access to cart contents
    - Basic validation of cart operations
    """

    def __init__(self):
        self._items = []

    @property
    def items(self):
        """Get list of all items in the cart as ProductQuantity objects"""
        return self._items[:]

    def add_item(self, product):
        """
        Add a single item to the cart.

        Args:
            product: Product to add
        """
        self.add_item_quantity(product, 1.0)

    def add_item_quantity(self, product, quantity):
        """
        Add a product with specified quantity to the cart.

        Args:
            product: Product to add
            quantity: Quantity of the product

        Raises:
            ValueError: If quantity is negative or product is None
        """
        if product is None:
            raise ValueError("Product cannot be None")

        if quantity < 0:
            raise ValueError("Quantity cannot be negative")

        # Allow zero quantity for edge cases, but validate it's not negative
        self._items.append(ProductQuantity(product, quantity))

    def remove_item(self, product):
        """
        Remove all instances of a product from the cart.

        Args:
            product: Product to remove

        Returns:
            int: Number of items removed
        """
        initial_count = len(self._items)
        self._items = [item for item in self._items if item.product != product]
        return initial_count - len(self._items)

    def clear(self):
        """Remove all items from the cart"""
        self._items.clear()

    def is_empty(self):
        """Check if the cart is empty"""
        return len(self._items) == 0

    def get_item_count(self):
        """Get total number of line items in the cart"""
        return len(self._items)

    def get_products(self):
        """Get set of unique products in the cart"""
        return set(item.product for item in self._items)
