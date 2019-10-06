import XCTest

@testable import SupermarketReceipt

class SupermarketReceiptTests: XCTestCase {

    private var catalog: SupermarketCatalog!
    private var teller: Teller!
    private var theCart: ShoppingCart!
    private var toothbrush: Product!
    private var rice: Product!
    private var apples: Product!
    private var cherryTomatoes: Product!

    override func setUp() {
        catalog = FakeCatalog() as! SupermarketCatalog
        teller = Teller(catalog: catalog)
        theCart = ShoppingCart()

        toothbrush = Product(name: "toothbrush", unit: ProductUnit.Each)
        catalog.addProduct(product: toothbrush, price: 0.99)
        rice = Product(name: "rice", unit: ProductUnit.Each)
        catalog.addProduct(product: rice, price: 2.99)
        apples = Product(name: "apples", unit: ProductUnit.Kilo)
        catalog.addProduct(product: apples, price: 1.99)
        cherryTomatoes = Product(name: "cherry tomato box", unit: ProductUnit.Each)
        catalog.addProduct(product: cherryTomatoes, price: 0.69)
    }

    
    func test_an_empty_shopping_cart_should_cost_nothing() {
        let receipt = teller.checksOutArticlesFrom(theCart: theCart)
//        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }

    
    func test_one_normal_item() {
        theCart.addItem(product: toothbrush)
        let receipt = teller.checksOutArticlesFrom(theCart: theCart)
//        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }

    
    func test_two_normal_items() {
        theCart.addItem(product: toothbrush)
        theCart.addItem(product: rice)
        let receipt = teller.checksOutArticlesFrom(theCart: theCart)
//        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }

    
    func test_buy_two_get_one_free() {
        theCart.addItem(product: toothbrush)
        theCart.addItem(product: toothbrush)
        theCart.addItem(product: toothbrush)
        teller.addSpecialOffer(offerType: SpecialOfferType.ThreeForTwo, product: toothbrush, argument: catalog.getUnitPrice(product: toothbrush))
        let receipt = teller.checksOutArticlesFrom(theCart: theCart)
//        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }

    
    func test_buy_two_get_one_free_but_insufficient_in_basket() {
        theCart.addItem(product: toothbrush)
        teller.addSpecialOffer(offerType: SpecialOfferType.ThreeForTwo, product: toothbrush, argument: catalog.getUnitPrice(product: toothbrush))
        let receipt = teller.checksOutArticlesFrom(theCart: theCart)
//        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }
    
    func test_buy_five_get_one_free() {
        theCart.addItem(product: toothbrush)
        theCart.addItem(product: toothbrush)
        theCart.addItem(product: toothbrush)
        theCart.addItem(product: toothbrush)
        theCart.addItem(product: toothbrush)
        teller.addSpecialOffer(offerType: SpecialOfferType.ThreeForTwo, product: toothbrush, argument: catalog.getUnitPrice(product: toothbrush))
        let receipt = teller.checksOutArticlesFrom(theCart: theCart)
//        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }

    
    func test_loose_weight_product() {
        theCart.addItemQuantity(product: apples, quantity: 0.5)
        let receipt = teller.checksOutArticlesFrom(theCart: theCart)
//        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }

    
    func test_percent_discount() {
        theCart.addItem(product: rice)
        teller.addSpecialOffer(offerType: SpecialOfferType.TenPercentDiscount, product: rice, argument: 10.0)
        let receipt = teller.checksOutArticlesFrom(theCart: theCart)
//        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }

    
    func test_xForY_discount() {
        theCart.addItem(product: cherryTomatoes)
        theCart.addItem(product: cherryTomatoes)
        teller.addSpecialOffer(offerType: SpecialOfferType.TwoForAmount, product: cherryTomatoes, argument: 0.99)
        let receipt = teller.checksOutArticlesFrom(theCart: theCart)
//        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }

    
    func test_xForY_discount_with_insufficient_in_basket() {
        theCart.addItem(product: cherryTomatoes)
        teller.addSpecialOffer(offerType: SpecialOfferType.TwoForAmount, product: cherryTomatoes, argument: 0.99)
        let receipt = teller.checksOutArticlesFrom(theCart: theCart)
//        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }

    
    func test_FiveForY_discount() {
        theCart.addItemQuantity(product: apples, quantity: 5)
        teller.addSpecialOffer(offerType: SpecialOfferType.FiveForAmount, product: apples,argument: 6.99)
        let receipt = teller.checksOutArticlesFrom(theCart: theCart)
//        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }

    
    func test_FiveForY_discount_withSix() {
        theCart.addItemQuantity(product: apples, quantity: 6)
        teller.addSpecialOffer(offerType: SpecialOfferType.FiveForAmount, product: apples,argument: 5.99)
        let receipt = teller.checksOutArticlesFrom(theCart: theCart)
//        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }

    
    func test_FiveForY_discount_withSixteen() {
        theCart.addItemQuantity(product: apples, quantity: 16)
        teller.addSpecialOffer(offerType: SpecialOfferType.FiveForAmount, product: apples,argument: 7.99)
        let receipt = teller.checksOutArticlesFrom(theCart: theCart)
//        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }

    
    func test_FiveForY_discount_withFour() {
        theCart.addItemQuantity(product: apples, quantity: 4)
        teller.addSpecialOffer(offerType: SpecialOfferType.FiveForAmount, product: apples,argument: 8.99)
        let receipt = teller.checksOutArticlesFrom(theCart: theCart)
//        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }
}
