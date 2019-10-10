package dojo.supermarket.model

import org.approvaltests.Approvals
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import supermarket.ReceiptPrinter
import supermarket.model.*

class SupermarketTest {
    private lateinit var catalog: SupermarketCatalog
    private lateinit var teller: Teller
    private lateinit var theCart: ShoppingCart
    private lateinit var toothbrush: Product
    private lateinit var rice: Product
    private lateinit var apples: Product
    private lateinit var cherryTomatoes: Product
    
    @BeforeEach
    fun setUp() {
        catalog = FakeCatalog()
        teller = Teller(catalog)
        theCart = ShoppingCart()

        toothbrush = Product("toothbrush", ProductUnit.Each)
        catalog.addProduct(toothbrush, 0.99)
        rice = Product("rice", ProductUnit.Each)
        catalog.addProduct(rice, 2.99)
        apples = Product("apples", ProductUnit.Kilo)
        catalog.addProduct(apples, 1.99)
        cherryTomatoes = Product("cherry tomato box", ProductUnit.Each)
        catalog.addProduct(cherryTomatoes, 0.69)

    }

    @Test
    fun an_empty_shopping_cart_should_cost_nothing() {
        val receipt = teller.checksOutArticlesFrom(theCart)
        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }

    @Test
    fun one_normal_item() {
        theCart.addItem(toothbrush)
        val receipt = teller.checksOutArticlesFrom(theCart)
        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }

    @Test
    fun two_normal_items() {
        theCart.addItem(toothbrush)
        theCart.addItem(rice)
        val receipt = teller.checksOutArticlesFrom(theCart)
        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }

    @Test
    fun buy_two_get_one_free() {
        theCart.addItem(toothbrush)
        theCart.addItem(toothbrush)
        theCart.addItem(toothbrush)
        teller.addSpecialOffer(SpecialOfferType.ThreeForTwo, toothbrush, catalog.getUnitPrice(toothbrush))
        val receipt = teller.checksOutArticlesFrom(theCart)
        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }

    @Test
    fun buy_five_get_one_free() {
        theCart.addItem(toothbrush)
        theCart.addItem(toothbrush)
        theCart.addItem(toothbrush)
        theCart.addItem(toothbrush)
        theCart.addItem(toothbrush)
        teller.addSpecialOffer(SpecialOfferType.ThreeForTwo, toothbrush, catalog.getUnitPrice(toothbrush))
        val receipt = teller.checksOutArticlesFrom(theCart)
        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }

    @Test
    fun loose_weight_product() {
        theCart.addItemQuantity(apples, .5)
        val receipt = teller.checksOutArticlesFrom(theCart)
        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }

    @Test
    fun percent_discount() {
        theCart.addItem(rice)
        teller.addSpecialOffer(SpecialOfferType.TenPercentDiscount, rice, 10.0)
        val receipt = teller.checksOutArticlesFrom(theCart)
        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }

    @Test
    fun xForY_discount() {
        theCart.addItem(cherryTomatoes)
        theCart.addItem(cherryTomatoes)
        teller.addSpecialOffer(SpecialOfferType.TwoForAmount, cherryTomatoes, .99)
        val receipt = teller.checksOutArticlesFrom(theCart)
        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }

    @Test
    fun FiveForY_discount() {
        theCart.addItemQuantity(apples, 5.0)
        teller.addSpecialOffer(SpecialOfferType.FiveForAmount, apples, 6.99)
        val receipt = teller.checksOutArticlesFrom(theCart)
        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }

    @Test
    fun FiveForY_discount_withSix() {
        theCart.addItemQuantity(apples, 6.0)
        teller.addSpecialOffer(SpecialOfferType.FiveForAmount, apples, 6.99)
        val receipt = teller.checksOutArticlesFrom(theCart)
        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }

    @Test
    fun FiveForY_discount_withSixteen() {
        theCart.addItemQuantity(apples, 16.0)
        teller.addSpecialOffer(SpecialOfferType.FiveForAmount, apples, 6.99)
        val receipt = teller.checksOutArticlesFrom(theCart)
        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }

    @Test
    fun FiveForY_discount_withFour() {
        theCart.addItemQuantity(apples, 4.0)
        teller.addSpecialOffer(SpecialOfferType.FiveForAmount, apples, 6.99)
        val receipt = teller.checksOutArticlesFrom(theCart)
        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }
}
