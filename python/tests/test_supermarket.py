import unittest

from domain import Product, SpecialOfferType, ProductUnit, ShoppingCart, Teller, FakeCatalog, BundleOffer


class SupermarketTest(unittest.TestCase):
    
    def setUp(self):
        """Set up common test fixtures"""
        self.catalog = FakeCatalog()
        self.teller = Teller(self.catalog)
        
        # Common products
        self.toothbrush = Product("toothbrush", ProductUnit.EACH)
        self.apples = Product("apples", ProductUnit.KILO)
        self.rice = Product("rice", ProductUnit.EACH)
        self.cherry_tomatoes = Product("cherry tomatoes", ProductUnit.KILO)
        self.pasta = Product("pasta", ProductUnit.EACH)
        
        # Add products to catalog with prices
        self.catalog.add_product(self.toothbrush, 0.99)
        self.catalog.add_product(self.apples, 1.99)
        self.catalog.add_product(self.rice, 2.49)
        self.catalog.add_product(self.cherry_tomatoes, 3.50)
        self.catalog.add_product(self.pasta, 1.20)
    
    def test_no_discount_when_no_offer_applied(self):
        """Test that items without offers are charged at full price"""
        cart = ShoppingCart()
        cart.add_item_quantity(self.apples, 2.5)

        receipt = self.teller.checks_out_articles_from(cart)

        self.assertAlmostEqual(receipt.total_price(), 4.975, places=2)
        self.assertEqual([], receipt.discounts)
        self.assertEqual(1, len(receipt.items))
    
    def test_ten_percent_discount_applied(self):
        """Test 10% discount is correctly applied"""
        self.teller.add_special_offer(SpecialOfferType.TEN_PERCENT_DISCOUNT, self.toothbrush, 10.0)

        cart = ShoppingCart()
        cart.add_item(self.toothbrush)

        receipt = self.teller.checks_out_articles_from(cart)

        # Price: 0.99, Discount: 0.099, Total: 0.891
        self.assertAlmostEqual(receipt.total_price(), 0.891, places=2)
        self.assertEqual(1, len(receipt.discounts))
        self.assertEqual("10.0% off", receipt.discounts[0].description)
        self.assertAlmostEqual(receipt.discounts[0].discount_amount, -0.099, places=3)
    
    def test_ten_percent_discount_with_multiple_items(self):
        """Test 10% discount with multiple items"""
        self.teller.add_special_offer(SpecialOfferType.TEN_PERCENT_DISCOUNT, self.toothbrush, 10.0)

        cart = ShoppingCart()
        cart.add_item(self.toothbrush)
        cart.add_item(self.toothbrush)
        cart.add_item(self.toothbrush)

        receipt = self.teller.checks_out_articles_from(cart)

        # Price: 0.99 * 3 = 2.97, Discount: 0.297, Total: 2.673
        self.assertAlmostEqual(receipt.total_price(), 2.673, places=2)
        self.assertEqual(1, len(receipt.discounts))
        self.assertAlmostEqual(receipt.discounts[0].discount_amount, -0.297, places=3)
    
    def test_three_for_two_with_exactly_three_items(self):
        """Test 3-for-2 offer with exactly 3 items"""
        self.teller.add_special_offer(SpecialOfferType.THREE_FOR_TWO, self.toothbrush, 0)

        cart = ShoppingCart()
        cart.add_item(self.toothbrush)
        cart.add_item(self.toothbrush)
        cart.add_item(self.toothbrush)

        receipt = self.teller.checks_out_articles_from(cart)

        # Pay for 2, get 3: 0.99 * 2 = 1.98
        self.assertAlmostEqual(receipt.total_price(), 1.98, places=2)
        self.assertEqual(1, len(receipt.discounts))
        self.assertEqual("3 for 2", receipt.discounts[0].description)
        self.assertAlmostEqual(receipt.discounts[0].discount_amount, -0.99, places=2)
    
    def test_three_for_two_with_four_items(self):
        """Test 3-for-2 offer with 4 items (should still only discount 1)"""
        self.teller.add_special_offer(SpecialOfferType.THREE_FOR_TWO, self.toothbrush, 0)

        cart = ShoppingCart()
        cart.add_item(self.toothbrush)
        cart.add_item(self.toothbrush)
        cart.add_item(self.toothbrush)
        cart.add_item(self.toothbrush)

        receipt = self.teller.checks_out_articles_from(cart)

        # Pay for 3, get 4: 0.99 * 3 = 2.97
        self.assertAlmostEqual(receipt.total_price(), 2.97, places=2)
        self.assertEqual(1, len(receipt.discounts))
    
    def test_three_for_two_with_six_items(self):
        """Test 3-for-2 offer with 6 items (2 complete sets)"""
        self.teller.add_special_offer(SpecialOfferType.THREE_FOR_TWO, self.toothbrush, 0)

        cart = ShoppingCart()
        for _ in range(6):
            cart.add_item(self.toothbrush)

        receipt = self.teller.checks_out_articles_from(cart)

        # Pay for 4, get 6: 0.99 * 4 = 3.96
        self.assertAlmostEqual(receipt.total_price(), 3.96, places=2)
        self.assertEqual(1, len(receipt.discounts))
        self.assertAlmostEqual(receipt.discounts[0].discount_amount, -1.98, places=2)
    
    def test_three_for_two_with_two_items_no_discount(self):
        """Test 3-for-2 offer with only 2 items (no discount should apply)"""
        self.teller.add_special_offer(SpecialOfferType.THREE_FOR_TWO, self.toothbrush, 0)

        cart = ShoppingCart()
        cart.add_item(self.toothbrush)
        cart.add_item(self.toothbrush)

        receipt = self.teller.checks_out_articles_from(cart)

        # Not enough for discount: 0.99 * 2 = 1.98
        self.assertAlmostEqual(receipt.total_price(), 1.98, places=2)
        self.assertEqual(0, len(receipt.discounts))
    
    def test_two_for_amount_with_exactly_two_items(self):
        """Test 2-for-X offer with exactly 2 items"""
        self.teller.add_special_offer(SpecialOfferType.TWO_FOR_AMOUNT, self.rice, 3.50)

        cart = ShoppingCart()
        cart.add_item(self.rice)
        cart.add_item(self.rice)

        receipt = self.teller.checks_out_articles_from(cart)

        # 2 for 3.50 instead of 2.49 * 2 = 4.98
        self.assertAlmostEqual(receipt.total_price(), 3.50, places=2)
        self.assertEqual(1, len(receipt.discounts))
        self.assertEqual("2 for 3.5", receipt.discounts[0].description)
        self.assertAlmostEqual(receipt.discounts[0].discount_amount, -1.48, places=2)
    
    def test_two_for_amount_with_three_items(self):
        """Test 2-for-X offer with 3 items (discount on 2, full price for 1)"""
        self.teller.add_special_offer(SpecialOfferType.TWO_FOR_AMOUNT, self.rice, 3.50)

        cart = ShoppingCart()
        cart.add_item(self.rice)
        cart.add_item(self.rice)
        cart.add_item(self.rice)

        receipt = self.teller.checks_out_articles_from(cart)

        # 2 for 3.50 + 1 at 2.49 = 5.99
        self.assertAlmostEqual(receipt.total_price(), 5.99, places=2)
        self.assertEqual(1, len(receipt.discounts))
    
    def test_two_for_amount_with_one_item_no_discount(self):
        """Test 2-for-X offer with only 1 item (no discount)"""
        self.teller.add_special_offer(SpecialOfferType.TWO_FOR_AMOUNT, self.rice, 3.50)

        cart = ShoppingCart()
        cart.add_item(self.rice)

        receipt = self.teller.checks_out_articles_from(cart)

        # Not enough for discount: 2.49
        self.assertAlmostEqual(receipt.total_price(), 2.49, places=2)
        self.assertEqual(0, len(receipt.discounts))
    
    def test_five_for_amount_with_exactly_five_items(self):
        """Test 5-for-X offer with exactly 5 items"""
        self.teller.add_special_offer(SpecialOfferType.FIVE_FOR_AMOUNT, self.pasta, 5.00)

        cart = ShoppingCart()
        for _ in range(5):
            cart.add_item(self.pasta)

        receipt = self.teller.checks_out_articles_from(cart)

        # 5 for 5.00 instead of 1.20 * 5 = 6.00
        self.assertAlmostEqual(receipt.total_price(), 5.00, places=2)
        self.assertEqual(1, len(receipt.discounts))
        self.assertEqual("5 for 5.0", receipt.discounts[0].description)
        self.assertAlmostEqual(receipt.discounts[0].discount_amount, -1.00, places=2)
    
    def test_five_for_amount_with_seven_items(self):
        """Test 5-for-X offer with 7 items"""
        self.teller.add_special_offer(SpecialOfferType.FIVE_FOR_AMOUNT, self.pasta, 5.00)

        cart = ShoppingCart()
        for _ in range(7):
            cart.add_item(self.pasta)

        receipt = self.teller.checks_out_articles_from(cart)

        # 5 for 5.00 + 2 at 1.20 = 7.40
        self.assertAlmostEqual(receipt.total_price(), 7.40, places=2)
        self.assertEqual(1, len(receipt.discounts))
    
    def test_five_for_amount_with_ten_items(self):
        """Test 5-for-X offer with 10 items (2 complete sets)"""
        self.teller.add_special_offer(SpecialOfferType.FIVE_FOR_AMOUNT, self.pasta, 5.00)

        cart = ShoppingCart()
        for _ in range(10):
            cart.add_item(self.pasta)

        receipt = self.teller.checks_out_articles_from(cart)

        # 2 sets of 5 for 5.00 = 10.00
        self.assertAlmostEqual(receipt.total_price(), 10.00, places=2)
        self.assertEqual(1, len(receipt.discounts))
        self.assertAlmostEqual(receipt.discounts[0].discount_amount, -2.00, places=2)
    
    def test_five_for_amount_with_four_items_no_discount(self):
        """Test 5-for-X offer with only 4 items (no discount)"""
        self.teller.add_special_offer(SpecialOfferType.FIVE_FOR_AMOUNT, self.pasta, 5.00)

        cart = ShoppingCart()
        for _ in range(4):
            cart.add_item(self.pasta)

        receipt = self.teller.checks_out_articles_from(cart)

        # Not enough for discount: 1.20 * 4 = 4.80
        self.assertAlmostEqual(receipt.total_price(), 4.80, places=2)
        self.assertEqual(0, len(receipt.discounts))
    
    def test_multiple_products_with_different_offers(self):
        """Test cart with multiple products having different offers"""
        self.teller.add_special_offer(SpecialOfferType.THREE_FOR_TWO, self.toothbrush, 0)
        self.teller.add_special_offer(SpecialOfferType.TEN_PERCENT_DISCOUNT, self.rice, 20.0)

        cart = ShoppingCart()
        # 3 toothbrushes (3-for-2)
        for _ in range(3):
            cart.add_item(self.toothbrush)
        # 2 rice (10% discount)
        cart.add_item(self.rice)
        cart.add_item(self.rice)

        receipt = self.teller.checks_out_articles_from(cart)

        # Toothbrush: 0.99 * 2 = 1.98
        # Rice: 2.49 * 2 = 4.98, discount 20% = 0.996, total = 3.984
        # Total: 1.98 + 3.984 = 5.964
        self.assertAlmostEqual(receipt.total_price(), 5.964, places=2)
        self.assertEqual(2, len(receipt.discounts))
    
    def test_mixed_products_some_with_offers_some_without(self):
        """Test cart with mixed products, some with offers and some without"""
        self.teller.add_special_offer(SpecialOfferType.TWO_FOR_AMOUNT, self.rice, 4.00)

        cart = ShoppingCart()
        cart.add_item_quantity(self.apples, 1.5)  # No offer
        cart.add_item(self.rice)  # With offer
        cart.add_item(self.rice)

        receipt = self.teller.checks_out_articles_from(cart)

        # Apples: 1.99 * 1.5 = 2.985
        # Rice: 2 for 4.00
        # Total: 2.985 + 4.00 = 6.985
        self.assertAlmostEqual(receipt.total_price(), 6.985, places=2)
        self.assertEqual(1, len(receipt.discounts))
    
    def test_add_item_increases_quantity(self):
        """Test that adding same item multiple times increases quantity"""
        cart = ShoppingCart()
        cart.add_item(self.toothbrush)
        cart.add_item(self.toothbrush)
        
        self.assertEqual(2, cart.product_quantities[self.toothbrush])
        self.assertEqual(2, len(cart.items))
    
    def test_add_item_quantity_directly(self):
        """Test adding item with specific quantity"""
        cart = ShoppingCart()
        cart.add_item_quantity(self.apples, 2.5)
        
        self.assertEqual(2.5, cart.product_quantities[self.apples])
        self.assertEqual(1, len(cart.items))
    
    def test_receipt_items_details(self):
        """Test receipt contains correct item details"""
        cart = ShoppingCart()
        cart.add_item_quantity(self.apples, 2.5)

        receipt = self.teller.checks_out_articles_from(cart)

        self.assertEqual(1, len(receipt.items))
        receipt_item = receipt.items[0]
        self.assertEqual(self.apples, receipt_item.product)
        self.assertEqual(1.99, receipt_item.price)
        self.assertAlmostEqual(receipt_item.total_price, 2.5 * 1.99, places=2)
        self.assertEqual(2.5, receipt_item.quantity)
    
    def test_bundle_discount_with_complete_bundle(self):
        """Test bundle discount when all items in bundle are present"""
        toothpaste = Product("toothpaste", ProductUnit.EACH)
        self.catalog.add_product(toothpaste, 1.79)
        
        # Create bundle: toothbrush + toothpaste
        bundle_products = [self.toothbrush, toothpaste]
        self.teller.add_offer(BundleOffer(bundle_products, 10.0))
        
        cart = ShoppingCart()
        cart.add_item(self.toothbrush)
        cart.add_item(toothpaste)
        
        receipt = self.teller.checks_out_articles_from(cart)
        
        # Total: 0.99 + 1.79 = 2.78
        # Discount: 10% of 2.78 = 0.278
        # Final: 2.78 - 0.278 = 2.502
        self.assertAlmostEqual(receipt.total_price(), 2.502, places=2)
        self.assertEqual(1, len(receipt.discounts))
        self.assertEqual("10.0% off bundle", receipt.discounts[0].description)
        self.assertAlmostEqual(receipt.discounts[0].discount_amount, -0.278, places=3)
    
    def test_bundle_discount_with_two_toothbrushes_one_toothpaste(self):
        """Test bundle discount with 2 toothbrushes and 1 toothpaste (only 1 complete bundle)"""
        toothpaste = Product("toothpaste", ProductUnit.EACH)
        self.catalog.add_product(toothpaste, 1.79)
        
        # Create bundle: toothbrush + toothpaste
        bundle_products = [self.toothbrush, toothpaste]
        self.teller.add_offer(BundleOffer(bundle_products, 10.0))
        
        cart = ShoppingCart()
        cart.add_item(self.toothbrush)
        cart.add_item(self.toothbrush)
        cart.add_item(toothpaste)
        
        receipt = self.teller.checks_out_articles_from(cart)
        
        # Total: 0.99 + 0.99 + 1.79 = 3.77
        # Discount: 10% of (0.99 + 1.79) = 0.278 (only 1 complete bundle)
        # Final: 3.77 - 0.278 = 3.492
        self.assertAlmostEqual(receipt.total_price(), 3.492, places=2)
        self.assertEqual(1, len(receipt.discounts))
        self.assertAlmostEqual(receipt.discounts[0].discount_amount, -0.278, places=3)
    
    def test_bundle_discount_with_two_complete_bundles(self):
        """Test bundle discount with 2 complete bundles"""
        toothpaste = Product("toothpaste", ProductUnit.EACH)
        self.catalog.add_product(toothpaste, 1.79)
        
        # Create bundle: toothbrush + toothpaste
        bundle_products = [self.toothbrush, toothpaste]
        self.teller.add_offer(BundleOffer(bundle_products, 10.0))
        
        cart = ShoppingCart()
        cart.add_item(self.toothbrush)
        cart.add_item(self.toothbrush)
        cart.add_item(toothpaste)
        cart.add_item(toothpaste)
        
        receipt = self.teller.checks_out_articles_from(cart)
        
        # Total: 0.99 * 2 + 1.79 * 2 = 5.56
        # Discount: 10% of 5.56 = 0.556 (2 complete bundles)
        # Final: 5.56 - 0.556 = 5.004
        self.assertAlmostEqual(receipt.total_price(), 5.004, places=2)
        self.assertEqual(1, len(receipt.discounts))
        self.assertAlmostEqual(receipt.discounts[0].discount_amount, -0.556, places=3)
    
    def test_bundle_discount_with_incomplete_bundle(self):
        """Test bundle discount when not all items in bundle are present"""
        toothpaste = Product("toothpaste", ProductUnit.EACH)
        self.catalog.add_product(toothpaste, 1.79)
        
        # Create bundle: toothbrush + toothpaste
        bundle_products = [self.toothbrush, toothpaste]
        self.teller.add_offer(BundleOffer(bundle_products, 10.0))
        
        cart = ShoppingCart()
        cart.add_item(self.toothbrush)
        # Missing toothpaste - incomplete bundle
        
        receipt = self.teller.checks_out_articles_from(cart)
        
        # Total: 0.99, no discount
        self.assertAlmostEqual(receipt.total_price(), 0.99, places=2)
        self.assertEqual(0, len(receipt.discounts))
    
    def test_bundle_discount_with_three_items_in_bundle(self):
        """Test bundle with 3 different items"""
        toothpaste = Product("toothpaste", ProductUnit.EACH)
        mouthwash = Product("mouthwash", ProductUnit.EACH)
        self.catalog.add_product(toothpaste, 1.79)
        self.catalog.add_product(mouthwash, 2.50)
        
        # Create bundle: toothbrush + toothpaste + mouthwash
        bundle_products = [self.toothbrush, toothpaste, mouthwash]
        self.teller.add_offer(BundleOffer(bundle_products, 10.0))
        
        cart = ShoppingCart()
        cart.add_item(self.toothbrush)
        cart.add_item(toothpaste)
        cart.add_item(mouthwash)
        
        receipt = self.teller.checks_out_articles_from(cart)
        
        # Total: 0.99 + 1.79 + 2.50 = 5.28
        # Discount: 10% of 5.28 = 0.528
        # Final: 5.28 - 0.528 = 4.752
        self.assertAlmostEqual(receipt.total_price(), 4.752, places=2)
        self.assertEqual(1, len(receipt.discounts))
        self.assertAlmostEqual(receipt.discounts[0].discount_amount, -0.528, places=3)
    
    def test_bundle_with_regular_offer_on_same_product(self):
        """Test bundle discount combined with regular offers"""
        toothpaste = Product("toothpaste", ProductUnit.EACH)
        self.catalog.add_product(toothpaste, 1.79)
        
        # Create bundle: toothbrush + toothpaste
        bundle_products = [self.toothbrush, toothpaste]
        self.teller.add_offer(BundleOffer(bundle_products, 10.0))
        
        # Also add a separate 10% discount on rice
        self.teller.add_special_offer(SpecialOfferType.TEN_PERCENT_DISCOUNT, self.rice, 20.0)
        
        cart = ShoppingCart()
        cart.add_item(self.toothbrush)
        cart.add_item(toothpaste)
        cart.add_item(self.rice)
        
        receipt = self.teller.checks_out_articles_from(cart)
        
        # Bundle: 0.99 + 1.79 = 2.78, discount 10% = 0.278
        # Rice: 2.49, discount 20% = 0.498
        # Total: 0.99 + 1.79 + 2.49 - 0.278 - 0.498 = 4.494
        self.assertAlmostEqual(receipt.total_price(), 4.494, places=2)
        self.assertEqual(2, len(receipt.discounts))
