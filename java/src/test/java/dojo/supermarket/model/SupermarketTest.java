package dojo.supermarket.model;

import dojo.supermarket.ReceiptPrinter;
import dojo.supermarket.ReceiptPrinterTest;
import org.approvaltests.Approvals;
import org.approvaltests.core.Options;
import org.approvaltests.scrubbers.RegExScrubber;

import static dojo.supermarket.ReceiptPrinterTest.verifyReceipt;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class SupermarketTest {
    private SupermarketCatalog catalog = new FakeCatalog();
    private Teller teller = new Teller(catalog);
    private ShoppingCart theCart = new ShoppingCart();
    private Product toothbrush = new Product("toothbrush", ProductUnit.Each);
    private Product rice = new Product("rice", ProductUnit.Each);
    private Product apples = new Product("apples", ProductUnit.Kilo);
    private Product cherryTomatoes = new Product("cherry tomato box", ProductUnit.Each);

    @BeforeEach
    public void setUp() {
        catalog.addProduct(toothbrush, 0.99);
        catalog.addProduct(rice, 2.99);
        catalog.addProduct(apples, 1.99);
        catalog.addProduct(cherryTomatoes, 0.69);
    }

    @Test
    public void an_empty_shopping_cart_should_cost_nothing() {
        Receipt receipt = teller.checksOutArticlesFrom(theCart);
        verifyReceipt(receipt);
    }

    @Test
    public void one_normal_item() {
        theCart.addItem(toothbrush);
        Receipt receipt = teller.checksOutArticlesFrom(theCart);
        verifyReceipt(receipt);
    }


    @Test
    public void two_normal_items() {
        theCart.addItem(toothbrush);
        theCart.addItem(rice);
        Receipt receipt = teller.checksOutArticlesFrom(theCart);
        verifyReceipt(receipt);
    }

    @Test
    public void buy_two_get_one_free() {
        theCart.addItem(toothbrush);
        theCart.addItem(toothbrush);
        theCart.addItem(toothbrush);
        teller.addSpecialOffer(SpecialOfferType.ThreeForTwo, toothbrush, catalog.getUnitPrice(toothbrush));
        Receipt receipt = teller.checksOutArticlesFrom(theCart);
        verifyReceipt(receipt);
    }

    @Test
    public void buy_two_get_one_free_but_insufficient_in_basket() {
        theCart.addItem(toothbrush);
        teller.addSpecialOffer(SpecialOfferType.ThreeForTwo, toothbrush, catalog.getUnitPrice(toothbrush));
        Receipt receipt = teller.checksOutArticlesFrom(theCart);
        verifyReceipt(receipt);
    }
    @Test
    public void buy_five_get_one_free() {
        theCart.addItem(toothbrush);
        theCart.addItem(toothbrush);
        theCart.addItem(toothbrush);
        theCart.addItem(toothbrush);
        theCart.addItem(toothbrush);
        teller.addSpecialOffer(SpecialOfferType.ThreeForTwo, toothbrush, catalog.getUnitPrice(toothbrush));
        Receipt receipt = teller.checksOutArticlesFrom(theCart);
        verifyReceipt(receipt);
    }

    @Test
    public void loose_weight_product() {
        theCart.addItemQuantity(apples, .5);
        Receipt receipt = teller.checksOutArticlesFrom(theCart);
        verifyReceipt(receipt);
    }

    @Test
    public void percent_discount() {
        theCart.addItem(rice);
        teller.addSpecialOffer(SpecialOfferType.TenPercentDiscount, rice, 10.0);
        Receipt receipt = teller.checksOutArticlesFrom(theCart);
        verifyReceipt(receipt);
    }

    @Test
    public void xForY_discount() {
        theCart.addItem(cherryTomatoes);
        theCart.addItem(cherryTomatoes);
        teller.addSpecialOffer(SpecialOfferType.TwoForAmount, cherryTomatoes,.99);
        Receipt receipt = teller.checksOutArticlesFrom(theCart);
        verifyReceipt(receipt);
    }

    @Test
    public void xForY_discount_with_insufficient_in_basket() {
        theCart.addItem(cherryTomatoes);
        teller.addSpecialOffer(SpecialOfferType.TwoForAmount, cherryTomatoes,.99);
        Receipt receipt = teller.checksOutArticlesFrom(theCart);
        verifyReceipt(receipt);
    }

    @Test
    public void FiveForY_discount() {
        theCart.addItemQuantity(apples, 5);
        teller.addSpecialOffer(SpecialOfferType.FiveForAmount, apples,6.99);
        Receipt receipt = teller.checksOutArticlesFrom(theCart);
        verifyReceipt(receipt);
    }

    @Test
    public void FiveForY_discount_withSix() {
        theCart.addItemQuantity(apples, 6);
        teller.addSpecialOffer(SpecialOfferType.FiveForAmount, apples,5.99);
        Receipt receipt = teller.checksOutArticlesFrom(theCart);
        verifyReceipt(receipt);
    }

    @Test
    public void FiveForY_discount_withSixteen() {
        theCart.addItemQuantity(apples, 16);
        teller.addSpecialOffer(SpecialOfferType.FiveForAmount, apples,7.99);
        Receipt receipt = teller.checksOutArticlesFrom(theCart);
        verifyReceipt(receipt);
    }

    @Test
    public void FiveForY_discount_withFour() {
        theCart.addItemQuantity(apples, 4);
        teller.addSpecialOffer(SpecialOfferType.FiveForAmount, apples,8.99);
        Receipt receipt = teller.checksOutArticlesFrom(theCart);
        verifyReceipt(receipt);
    }
}
