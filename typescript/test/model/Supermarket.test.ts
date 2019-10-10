import {FakeCatalog} from "./FakeCatalog"
import {Product} from "../../src/model/Product"
import {SupermarketCatalog} from "../../src/model/SupermarketCatalog"
import {Receipt} from "../../src/model/Receipt"
import {ShoppingCart} from "../../src/model/ShoppingCart"
import {Teller} from "../../src/model/Teller"
import {SpecialOfferType} from "../../src/model/SpecialOfferType"
import {ProductUnit} from "../../src/model/ProductUnit"
import {ReceiptPrinter} from "../../src/ReceiptPrinter"
import {expect} from 'chai'
import AsyncFunc = Mocha.AsyncFunc
const approvals = require('approvals')

type Approvals = { verify: (a: string) => void }
describe('Supermarket', function () {

    approvals.mocha()

    let catalog: SupermarketCatalog;
    let teller: Teller;
    let theCart: ShoppingCart;
    let toothbrush: Product;
    let rice: Product;
    let apples: Product;
    let cherryTomatoes: Product;

    beforeEach(function () {

        catalog = new FakeCatalog();
        teller = new Teller(catalog);
        theCart = new ShoppingCart();

        toothbrush = new Product("toothbrush", ProductUnit.Each);
        catalog.addProduct(toothbrush, 0.99);
        rice = new Product("rice", ProductUnit.Each);
        catalog.addProduct(rice, 2.99);
        apples = new Product("apples", ProductUnit.Kilo);
        catalog.addProduct(apples, 1.99);
        cherryTomatoes = new Product("cherry tomato box", ProductUnit.Each);
        catalog.addProduct(cherryTomatoes, 0.69);

    });


    it('an_empty_shopping_cart_should_cost_nothing', function (this: any) {
        const receipt = teller.checksOutArticlesFrom(theCart);
        this.verify(new ReceiptPrinter(40).printReceipt(receipt));
    });


    it('one_normal_item',function (this: any) {
        theCart.addItem(toothbrush);
        const receipt = teller.checksOutArticlesFrom(theCart);
        this.verify(new ReceiptPrinter(40).printReceipt(receipt));
    })


    it('two_normal_items',function (this: any) {
        theCart.addItem(toothbrush);
        theCart.addItem(rice);
        const receipt = teller.checksOutArticlesFrom(theCart);
        this.verify(new ReceiptPrinter(40).printReceipt(receipt));
    })


    it('buy_two_get_one_free',function (this: any) {
        theCart.addItem(toothbrush);
        theCart.addItem(toothbrush);
        theCart.addItem(toothbrush);
        teller.addSpecialOffer(SpecialOfferType.ThreeForTwo, toothbrush, catalog.getUnitPrice(toothbrush));
        const receipt = teller.checksOutArticlesFrom(theCart);
        expect(receipt.getDiscounts()).lengthOf(1)

        this.verify(new ReceiptPrinter(40).printReceipt(receipt));
    })


    it('buy_five_get_one_free',function (this: any) {
        theCart.addItem(toothbrush);
        theCart.addItem(toothbrush);
        theCart.addItem(toothbrush);
        theCart.addItem(toothbrush);
        theCart.addItem(toothbrush);
        teller.addSpecialOffer(SpecialOfferType.ThreeForTwo, toothbrush, catalog.getUnitPrice(toothbrush));
        const receipt = teller.checksOutArticlesFrom(theCart);
        this.verify (new ReceiptPrinter(40).printReceipt(receipt));
    })


    it('loose_weight_product',function (this: any) {
        theCart.addItemQuantity(apples, .5);
        const receipt = teller.checksOutArticlesFrom(theCart);
        this.verify(new ReceiptPrinter(40).printReceipt(receipt));
    })


    it('percent_discount',function (this: any) {
        theCart.addItem(rice);
        teller.addSpecialOffer(SpecialOfferType.TenPercentDiscount, rice, 10.0);
        const receipt = teller.checksOutArticlesFrom(theCart);
        expect(receipt.getDiscounts()).lengthOf(1)

        this.verify(new ReceiptPrinter(40).printReceipt(receipt));
    })


    it('xForY_discount',function (this: any) {
        theCart.addItem(cherryTomatoes);
        theCart.addItem(cherryTomatoes);
        teller.addSpecialOffer(SpecialOfferType.TwoForAmount, cherryTomatoes, .99);
        const receipt = teller.checksOutArticlesFrom(theCart);
        expect(receipt.getDiscounts()).lengthOf(1)

        this.verify(new ReceiptPrinter(40).printReceipt(receipt));
    })

    it('FiveForY_discount', function (this: any) {
        theCart.addItemQuantity(apples, 5);
        teller.addSpecialOffer(SpecialOfferType.FiveForAmount, apples, 6.99);
        const receipt = teller.checksOutArticlesFrom(theCart);
        this.verify(new ReceiptPrinter(40).printReceipt(receipt));
    });

    it('FiveForY_discount_withSix',function (this: any) {
        theCart.addItemQuantity(apples, 6);
        teller.addSpecialOffer(SpecialOfferType.FiveForAmount, apples, 6.99);
        const receipt = teller.checksOutArticlesFrom(theCart);
        this.verify(new ReceiptPrinter(40).printReceipt(receipt));
    })


    it('FiveForY_discount_withSixteen',function (this: any) {
        theCart.addItemQuantity(apples, 16);
        teller.addSpecialOffer(SpecialOfferType.FiveForAmount, apples, 6.99);
        const receipt = teller.checksOutArticlesFrom(theCart);
        this.verify(new ReceiptPrinter(40).printReceipt(receipt));
    })


    it('FiveForY_discount_withFour',function (this: any) {
        theCart.addItemQuantity(apples, 4);
        teller.addSpecialOffer(SpecialOfferType.FiveForAmount, apples, 6.99);
        const receipt = teller.checksOutArticlesFrom(theCart);

        let receiptPrinter = new ReceiptPrinter(40)
        this.verify(receiptPrinter.printReceipt(receipt));
    })

});
