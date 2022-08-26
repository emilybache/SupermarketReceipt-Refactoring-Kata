package dojo.supermarket.model;

import dojo.supermarket.ReceiptPrinter;
import org.approvaltests.Approvals;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class SupermarketTest {
    private SupermarketCatalog catalog;
    private Teller teller;
    private ShoppingCart theCart;
    private Product toothbrush;
    private Product rice;
    private Product apples;
    private Product cherryTomatoes;

    @BeforeEach
    public void setUp() {
        catalog = new FakeCatalog();
        teller = new Teller(catalog);
        theCart = new ShoppingCart();

        toothbrush = new Product("toothbrush", ProductUnit.EACH);
        catalog.addProduct(toothbrush, 0.99);
        rice = new Product("rice", ProductUnit.EACH);
        catalog.addProduct(rice, 2.99);
        apples = new Product("apples", ProductUnit.KILO);
        catalog.addProduct(apples, 1.99);
        cherryTomatoes = new Product("cherry tomato box", ProductUnit.EACH);
        catalog.addProduct(cherryTomatoes, 0.69);

    }

    @Test
    public void an_empty_shopping_cart_should_cost_nothing() {
        Receipt receipt = teller.checksOutArticlesFrom(theCart);
        Approvals.verify(new ReceiptPrinter(40).printReceipt(receipt));
    }

    @Test
    public void one_normal_item() {
        theCart.addItem(toothbrush);
        Receipt receipt = teller.checksOutArticlesFrom(theCart);
        Approvals.verify(new ReceiptPrinter(40).printReceipt(receipt));
    }

    @Test
    public void two_normal_items() {
        theCart.addItem(toothbrush);
        theCart.addItem(rice);
        Receipt receipt = teller.checksOutArticlesFrom(theCart);
        Approvals.verify(new ReceiptPrinter(40).printReceipt(receipt));
    }

    @Test
    public void buy_two_get_one_free() {
        theCart.addItem(toothbrush);
        theCart.addItem(toothbrush);
        theCart.addItem(toothbrush);
        teller.addSpecialOffer(SpecialOfferType.THREE_FOR_TWO, toothbrush, catalog.getUnitPrice(toothbrush));
        Receipt receipt = teller.checksOutArticlesFrom(theCart);
        Approvals.verify(new ReceiptPrinter(40).printReceipt(receipt));
    }

    @Test
    public void buy_two_get_one_free_but_insufficient_in_basket() {
        theCart.addItem(toothbrush);
        teller.addSpecialOffer(SpecialOfferType.THREE_FOR_TWO, toothbrush, catalog.getUnitPrice(toothbrush));
        Receipt receipt = teller.checksOutArticlesFrom(theCart);
        Approvals.verify(new ReceiptPrinter(40).printReceipt(receipt));
    }
    @Test
    public void buy_five_get_one_free() {
        theCart.addItem(toothbrush);
        theCart.addItem(toothbrush);
        theCart.addItem(toothbrush);
        theCart.addItem(toothbrush);
        theCart.addItem(toothbrush);
        teller.addSpecialOffer(SpecialOfferType.THREE_FOR_TWO, toothbrush, catalog.getUnitPrice(toothbrush));
        Receipt receipt = teller.checksOutArticlesFrom(theCart);
        Approvals.verify(new ReceiptPrinter(40).printReceipt(receipt));
    }

    @Test
    public void loose_weight_product() {
        theCart.addItemQuantity(apples, .5);
        Receipt receipt = teller.checksOutArticlesFrom(theCart);
        Approvals.verify(new ReceiptPrinter(40).printReceipt(receipt));
    }

    @Test
    public void percent_discount() {
        theCart.addItem(rice);
        teller.addSpecialOffer(SpecialOfferType.TEN_PERCENT_DISCOUNT, rice, 10.0);
        Receipt receipt = teller.checksOutArticlesFrom(theCart);
        Approvals.verify(new ReceiptPrinter(40).printReceipt(receipt));
    }

    @Test
    public void xForY_discount() {
        theCart.addItem(cherryTomatoes);
        theCart.addItem(cherryTomatoes);
        teller.addSpecialOffer(SpecialOfferType.TWO_FOR_AMOUNT, cherryTomatoes,.99);
        Receipt receipt = teller.checksOutArticlesFrom(theCart);
        Approvals.verify(new ReceiptPrinter(40).printReceipt(receipt));
    }

    @Test
    public void xForY_discount_with_insufficient_in_basket() {
        theCart.addItem(cherryTomatoes);
        teller.addSpecialOffer(SpecialOfferType.TWO_FOR_AMOUNT, cherryTomatoes,.99);
        Receipt receipt = teller.checksOutArticlesFrom(theCart);
        Approvals.verify(new ReceiptPrinter(40).printReceipt(receipt));
    }

    @Test
    public void FiveForY_discount() {
        theCart.addItemQuantity(apples, 5);
        teller.addSpecialOffer(SpecialOfferType.FIVE_FOR_AMOUNT, apples,6.99);
        Receipt receipt = teller.checksOutArticlesFrom(theCart);
        Approvals.verify(new ReceiptPrinter(40).printReceipt(receipt));
    }

    @Test
    public void FiveForY_discount_withSix() {
        theCart.addItemQuantity(apples, 6);
        teller.addSpecialOffer(SpecialOfferType.FIVE_FOR_AMOUNT, apples,5.99);
        Receipt receipt = teller.checksOutArticlesFrom(theCart);
        Approvals.verify(new ReceiptPrinter(40).printReceipt(receipt));
    }

    @Test
    public void FiveForY_discount_withSixteen() {
        theCart.addItemQuantity(apples, 16);
        teller.addSpecialOffer(SpecialOfferType.FIVE_FOR_AMOUNT, apples,7.99);
        Receipt receipt = teller.checksOutArticlesFrom(theCart);
        Approvals.verify(new ReceiptPrinter(40).printReceipt(receipt));
    }

    @Test
    public void FiveForY_discount_withFour() {
        theCart.addItemQuantity(apples, 4);
        teller.addSpecialOffer(SpecialOfferType.FIVE_FOR_AMOUNT, apples,8.99);
        Receipt receipt = teller.checksOutArticlesFrom(theCart);
        Approvals.verify(new ReceiptPrinter(40).printReceipt(receipt));
    }
}
