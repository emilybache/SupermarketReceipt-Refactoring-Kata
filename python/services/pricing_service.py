class PricingService:
    """
    Service responsible for pricing calculations and product management.

    This service handles:
    - Converting cart items to product quantities
    - Calculating line item prices
    - Validating pricing inputs
    """

    def __init__(self, catalog):
        self.catalog = catalog

    def calculate_line_item_price(self, product, quantity):
        """
        Calculate the total price for a line item.

        Args:
            product: Product object
            quantity: Quantity of the product

        Returns:
            tuple: (unit_price, total_price)
        """
        unit_price = self.catalog.unit_price(product)
        total_price = quantity * unit_price
        return unit_price, total_price

    def aggregate_product_quantities(self, cart_items):
        """
        Aggregate cart items into product quantities.

        Args:
            cart_items: List of ProductQuantity objects from cart

        Returns:
            dict: Mapping of products to their total quantities
        """
        product_quantities = {}

        for item in cart_items:
            product = item.product
            quantity = item.quantity

            if product in product_quantities:
                product_quantities[product] += quantity
            else:
                product_quantities[product] = quantity

        return product_quantities

    def validate_pricing_inputs(self, product, quantity):
        """
        Validate pricing inputs.

        Args:
            product: Product object
            quantity: Quantity of the product

        Raises:
            ValueError: If inputs are invalid
        """
        if not product:
            raise ValueError("Product cannot be None")

        if quantity < 0:
            raise ValueError("Quantity cannot be negative")

        # - Check if product exists in catalog
        if self.catalog.unit_price(product) is None:
            raise ValueError(f"Product {product} not found in catalog")

        # - Validate quantity makes sense for product unit type
        if product.unit == "each" and not quantity.is_integer():
            raise ValueError("Quantity must be an integer for 'each' products")

        # - Check for maximum quantity limits
        if quantity > 1000:
            raise ValueError("Quantity exceeds maximum limit of 1000")
