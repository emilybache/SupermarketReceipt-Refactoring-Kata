import XCTest

@testable import SupermarketReceipt

class SupermarketReceiptTests: XCTestCase {

    private var catalog: SupermarketCatalog!
    private lazy var teller: Teller!
    private lazy var theCart: ShoppingCart!
    private lazy var toothbrush: Product!
    private lazy var apples: Product!
    private lazy var cherryTomatoes: Product!

    override func setUp() {
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

    
    func test_an_empty_shopping_cart_should_cost_nothing() {
        let receipt = teller.checksOutArticlesFrom(theCart)
        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }

    
    func test_one_normal_item() {
        theCart.addItem(toothbrush)
        let receipt = teller.checksOutArticlesFrom(theCart)
        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }

    
    func test_two_normal_items() {
        theCart.addItem(toothbrush)
        theCart.addItem(rice)
        let receipt = teller.checksOutArticlesFrom(theCart)
        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }

    
    func test_buy_two_get_one_free() {
        theCart.addItem(toothbrush)
        theCart.addItem(toothbrush)
        theCart.addItem(toothbrush)
        teller.addSpecialOffer(SpecialOfferType.ThreeForTwo, toothbrush, catalog.getUnitPrice(toothbrush))
        let receipt = teller.checksOutArticlesFrom(theCart)
        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }

    
    func test_buy_two_get_one_free_but_insufficient_in_basket() {
        theCart.addItem(toothbrush)
        teller.addSpecialOffer(SpecialOfferType.ThreeForTwo, toothbrush, catalog.getUnitPrice(toothbrush))
        let receipt = teller.checksOutArticlesFrom(theCart)
        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }
    
    func test_buy_five_get_one_free() {
        theCart.addItem(toothbrush)
        theCart.addItem(toothbrush)
        theCart.addItem(toothbrush)
        theCart.addItem(toothbrush)
        theCart.addItem(toothbrush)
        teller.addSpecialOffer(SpecialOfferType.ThreeForTwo, toothbrush, catalog.getUnitPrice(toothbrush))
        let receipt = teller.checksOutArticlesFrom(theCart)
        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }

    
    func test_loose_weight_product() {
        theCart.addItemQuantity(apples, 0.5)
        let receipt = teller.checksOutArticlesFrom(theCart)
        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }

    
    func test_percent_discount() {
        theCart.addItem(rice)
        teller.addSpecialOffer(SpecialOfferType.TenPercentDiscount, rice, 10.0)
        let receipt = teller.checksOutArticlesFrom(theCart)
        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }

    
    func test_xForY_discount() {
        theCart.addItem(cherryTomatoes)
        theCart.addItem(cherryTomatoes)
        teller.addSpecialOffer(SpecialOfferType.TwoForAmount, cherryTomatoes, 0.99)
        let receipt = teller.checksOutArticlesFrom(theCart)
        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }

    
    func test_xForY_discount_with_insufficient_in_basket() {
        theCart.addItem(cherryTomatoes)
        teller.addSpecialOffer(SpecialOfferType.TwoForAmount, cherryTomatoes, 0.99)
        let receipt = teller.checksOutArticlesFrom(theCart)
        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }

    
    func test_FiveForY_discount() {
        theCart.addItemQuantity(apples, 5)
        teller.addSpecialOffer(SpecialOfferType.FiveForAmount, apples,6.99)
        let receipt = teller.checksOutArticlesFrom(theCart)
        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }

    
    func test_FiveForY_discount_withSix() {
        theCart.addItemQuantity(apples, 6)
        teller.addSpecialOffer(SpecialOfferType.FiveForAmount, apples,5.99)
        let receipt = teller.checksOutArticlesFrom(theCart)
        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }

    
    func test_FiveForY_discount_withSixteen() {
        theCart.addItemQuantity(apples, 16)
        teller.addSpecialOffer(SpecialOfferType.FiveForAmount, apples,7.99)
        let receipt = teller.checksOutArticlesFrom(theCart)
        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }

    
    func test_FiveForY_discount_withFour() {
        theCart.addItemQuantity(apples, 4)
        teller.addSpecialOffer(SpecialOfferType.FiveForAmount, apples,8.99)
        let receipt = teller.checksOutArticlesFrom(theCart)
        Approvals.verify(ReceiptPrinter(40).printReceipt(receipt))
    }
}
