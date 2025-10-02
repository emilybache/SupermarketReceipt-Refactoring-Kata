package dojo.supermarket.model

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import supermarket.model.*
import kotlin.collections.listOf


class SupermarketTest {

    @Test
    fun tenPercentDiscount() {
        val catalog = FakeCatalog()
        val toothbrush = Product("toothbrush", ProductUnit.Each)
        catalog.addProduct(toothbrush, 0.99)
        val apples = Product("apples", ProductUnit.Kilo)
        catalog.addProduct(apples, 1.99)

        val teller = Teller(catalog)
        teller.addSpecialOffer(SpecialOfferType.TenPercentDiscount, toothbrush, 10.0)

        val cart = ShoppingCart()
        cart.addItemQuantity(apples, 2.5)

        // ACT
        val receipt = teller.checksOutArticlesFrom(cart)

        // ASSERT
        assertEquals(4.975, receipt.totalPrice, 0.01)
        assertEquals(emptyList<Discount>(), receipt.getDiscounts())
        assertEquals(1, receipt.getItems().size)
        val receiptItem = receipt.getItems().get(0)
        assertEquals(apples, receiptItem.product)
        assertEquals(1.99, receiptItem.price)
        assertEquals(2.5 * 1.99, receiptItem.totalPrice)
        assertEquals(2.5, receiptItem.quantity)
    }
}
