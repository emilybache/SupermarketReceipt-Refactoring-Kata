import {FakeCatalog} from "./FakeCatalog"
import {Product} from "../src/model/Product"
import {SupermarketCatalog} from "../src/model/SupermarketCatalog"
import {Receipt} from "../src/model/Receipt"
import {ShoppingCart} from "../src/model/ShoppingCart"
import {Teller} from "../src/model/Teller"
import {SpecialOfferType} from "../src/model/SpecialOfferType"
import {ProductUnit} from "../src/model/ProductUnit"
const approvals = require('approvals')

type Approvals = {
    verify: (s: string) => void
    verifyAsJSON: (o: Object) => void
}
describe('Supermarket', function () {

    approvals.mocha()
    it('TODO decide what to specify', function (this: any) {

        const catalog: SupermarketCatalog = new FakeCatalog();
        const toothbrush: Product = new Product("toothbrush", ProductUnit.Each);
        catalog.addProduct(toothbrush, 0.99);
        const apples: Product = new Product("apples", ProductUnit.Kilo);
        catalog.addProduct(apples, 1.99);

        const cart: ShoppingCart = new ShoppingCart();
        cart.addItemQuantity(apples, 2.5);

        const teller: Teller = new Teller(catalog);
        teller.addSpecialOffer(SpecialOfferType.TenPercentDiscount, toothbrush, 10.0);

        const receipt: Receipt = teller.checksOutArticlesFrom(cart);

        // Todo: complete this test
        this.verifyAsJSON({})
    });

});
