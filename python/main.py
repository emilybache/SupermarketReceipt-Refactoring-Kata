from python.domain.model_objects import Product, SpecialOfferType, ProductUnit
from python.shopping_cart import ShoppingCart
from python.teller import Teller
from python.infrastructure.receipt_printer import ReceiptPrinter
from python.tests.fake_catalog import FakeCatalog


def setup_catalog_and_products():
    """Setup the catalog with sample products"""
    catalog = FakeCatalog()

    # Create products
    toothbrush = Product("toothbrush", ProductUnit.EACH)
    apples = Product("apples", ProductUnit.KILO)
    toothpaste = Product("toothpaste", ProductUnit.EACH)
    cherry_tomatoes = Product("cherry tomatoes", ProductUnit.EACH)
    rice = Product("rice", ProductUnit.EACH)

    # Add to catalog with prices
    catalog.add_product(toothbrush, 0.99)
    catalog.add_product(apples, 1.99)
    catalog.add_product(toothpaste, 1.79)
    catalog.add_product(cherry_tomatoes, 0.69)
    catalog.add_product(rice, 2.49)

    return catalog, {
        'toothbrush': toothbrush,
        'apples': apples,
        'toothpaste': toothpaste,
        'cherry_tomatoes': cherry_tomatoes,
        'rice': rice
    }


def setup_special_offers(teller, products):
    """Setup special offers for demonstration"""
    teller.add_special_offer(
        SpecialOfferType.THREE_FOR_TWO,
        products['toothbrush'],
        0
    )

    teller.add_special_offer(
        SpecialOfferType.TEN_PERCENT_DISCOUNT,
        products['apples'],
        20.0
    )

    teller.add_special_offer(
        SpecialOfferType.FIVE_FOR_AMOUNT,
        products['toothpaste'],
        7.49
    )

    teller.add_special_offer(
        SpecialOfferType.TWO_FOR_AMOUNT,
        products['cherry_tomatoes'],
        0.99
    )


def demo_simple_checkout(teller, products, printer):
    """Demonstrate a simple checkout without discounts"""
    print("=== Simple Checkout (No Discounts) ===")

    cart = ShoppingCart()
    cart.add_item(products['rice'])
    cart.add_item_quantity(products['apples'], 1.0)  # Not enough for discount

    total_before = teller.calculate_total_before_discounts(cart)
    receipt = teller.checks_out_articles_from(cart)

    print(f"Total before discounts: €{total_before:.2f}")
    print(f"Total after discounts: €{receipt.total_price():.2f}")
    print(f"Savings: €{total_before - receipt.total_price():.2f}")
    print("\nReceipt:")
    print(printer.print_receipt(receipt))


def demo_discount_checkout(teller, products, printer):
    """Demonstrate checkout with multiple discounts"""
    print("=== Discount Checkout (Multiple Offers) ===")

    cart = ShoppingCart()
    cart.add_item_quantity(products['toothbrush'], 3)    # 3-for-2
    cart.add_item_quantity(products['apples'], 2.5)     # 20% off
    cart.add_item_quantity(products['toothpaste'], 5)   # 5 for €7.49
    cart.add_item_quantity(products['cherry_tomatoes'], 4)  # 2 for €0.99

    total_before = teller.calculate_total_before_discounts(cart)
    receipt = teller.checks_out_articles_from(cart)

    print(f"Total before discounts: €{total_before:.2f}")
    print(f"Total after discounts: €{receipt.total_price():.2f}")
    print(f"Savings: €{total_before - receipt.total_price():.2f}")
    print(f"Number of discounts applied: {len(receipt.discounts)}")
    print("\nReceipt:")
    print(printer.print_receipt(receipt))


def demo_complex_checkout(teller, products, printer):
    """Demonstrate a complex checkout scenario"""
    print("=== Complex Checkout Scenario ===")

    cart = ShoppingCart()
    cart.add_item_quantity(products['toothbrush'], 7)    # 2 sets of 3-for-2
    cart.add_item_quantity(products['toothpaste'], 8)    # 5 for special + 3 regular
    cart.add_item_quantity(products['apples'], 3.2)     # 20% off
    cart.add_item(products['rice'])                     # No discount

    total_before = teller.calculate_total_before_discounts(cart)
    receipt = teller.checks_out_articles_from(cart)

    print(f"Items in cart: {cart.get_item_count()}")
    print(f"Unique products: {len(cart.get_products())}")
    print(f"Total before discounts: €{total_before:.2f}")
    print(f"Total after discounts: €{receipt.total_price():.2f}")
    print(f"Total savings: €{total_before - receipt.total_price():.2f}")
    print(f"Discounts applied: {len(receipt.discounts)}")

    print("\nDiscount details:")
    for discount in receipt.discounts:
        print(f"  - {discount.description}: €{-discount.discount_amount:.2f}")

    print("\nFull Receipt:")
    print(printer.print_receipt(receipt))


def main():
    """
       Main entry point for the Supermarket Receipt system.
       This script demonstrates the refactored system with proper architecture
       following Domain-Driven Design and Clean Architecture principles.
    """
    print("Supermarket Receipt System - Refactored Architecture Demo")
    print("=" * 60)

    # Setup system components
    catalog, products = setup_catalog_and_products()
    teller = Teller(catalog)
    printer = ReceiptPrinter(columns=50)

    # Setup special offers
    setup_special_offers(teller, products)

    print("\nActive Special Offers:")
    offers = teller.get_special_offers()
    for product, offer in offers.items():
        strategy_names = {
            SpecialOfferType.THREE_FOR_TWO: "3 for 2",
            SpecialOfferType.TEN_PERCENT_DISCOUNT: f"{offer.argument}% off",
            SpecialOfferType.TWO_FOR_AMOUNT: f"2 for €{offer.argument}",
            SpecialOfferType.FIVE_FOR_AMOUNT: f"5 for €{offer.argument}"
        }
        offer_name = strategy_names.get(offer.offer_type, "Unknown")
        print(f"  - {product.name}: {offer_name}")

    print("\n" + "=" * 60)

    # Run demonstrations
    demo_simple_checkout(teller, products, printer)
    demo_discount_checkout(teller, products, printer)
    demo_complex_checkout(teller, products, printer)


if __name__ == "__main__":
    main()
