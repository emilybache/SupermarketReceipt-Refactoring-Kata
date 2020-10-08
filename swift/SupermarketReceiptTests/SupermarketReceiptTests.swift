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
        catalog = FakeCatalog() as SupermarketCatalog
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

        let result = ReceiptPrinter(columns: 40).printReceipt(receipt: receipt)

        let expected = "\nTotal:                              0.00"
        XCTAssertEqual(expected, result)
    }

    func test_one_normal_item() {
        theCart.addItem(product: toothbrush)
        let receipt = teller.checksOutArticlesFrom(theCart: theCart)
        let result = ReceiptPrinter(columns: 40).printReceipt(receipt: receipt)
        
        let expected =
            """
            toothbrush                          0.99

            Total:                              0.99
            """
        XCTAssertEqual(expected, result)
    }

    func test_two_normal_items() {
        theCart.addItem(product: toothbrush)
        theCart.addItem(product: rice)
        let receipt = teller.checksOutArticlesFrom(theCart: theCart)
        let result = ReceiptPrinter(columns: 40).printReceipt(receipt: receipt)

        let expected =
            """
            toothbrush                          0.99
            rice                                2.99

            Total:                              3.98
            """
        XCTAssertEqual(expected, result)
    }
    
    func test_buy_two_get_one_free() {
        theCart.addItem(product: toothbrush)
        theCart.addItem(product: toothbrush)
        theCart.addItem(product: toothbrush)
        teller.addSpecialOffer(offerType: SpecialOfferType.ThreeForTwo, product: toothbrush, argument: catalog.getUnitPrice(product: toothbrush))
        let receipt = teller.checksOutArticlesFrom(theCart: theCart)
        
        let result = ReceiptPrinter(columns: 40).printReceipt(receipt: receipt)

        let expected =
            """
            toothbrush                          0.99
            toothbrush                          0.99
            toothbrush                          0.99
            3 for 2(toothbrush)                -0.99

            Total:                              1.98
            """
        XCTAssertEqual(expected, result)
    }
    
    func test_buy_two_get_one_free_but_insufficient_in_basket() {
        theCart.addItem(product: toothbrush)
        teller.addSpecialOffer(offerType: SpecialOfferType.ThreeForTwo, product: toothbrush, argument: catalog.getUnitPrice(product: toothbrush))
        let receipt = teller.checksOutArticlesFrom(theCart: theCart)
        
        let result = ReceiptPrinter(columns: 40).printReceipt(receipt: receipt)

        let expected =
            """
            toothbrush                          0.99

            Total:                              0.99
            """
        XCTAssertEqual(expected, result)
    }
    
    func test_buy_five_get_one_free() {
        theCart.addItem(product: toothbrush)
        theCart.addItem(product: toothbrush)
        theCart.addItem(product: toothbrush)
        theCart.addItem(product: toothbrush)
        theCart.addItem(product: toothbrush)
        teller.addSpecialOffer(offerType: SpecialOfferType.ThreeForTwo, product: toothbrush, argument: catalog.getUnitPrice(product: toothbrush))
        let receipt = teller.checksOutArticlesFrom(theCart: theCart)

        let result = ReceiptPrinter(columns: 40).printReceipt(receipt: receipt)

        let expected =
            """
            toothbrush                          0.99
            toothbrush                          0.99
            toothbrush                          0.99
            toothbrush                          0.99
            toothbrush                          0.99
            3 for 2(toothbrush)                -0.99

            Total:                              3.96
            """
        XCTAssertEqual(expected, result)
    }

    func test_loose_weight_product() {
        theCart.addItemQuantity(product: apples, quantity: 0.5)
        let receipt = teller.checksOutArticlesFrom(theCart: theCart)

        let result = ReceiptPrinter(columns: 40).printReceipt(receipt: receipt)
        
        let expected =
            """
            apples                              1.00
              1.99 * 0.500

            Total:                              1.00
            """
        
        XCTAssertEqual(expected, result)
    }
    
    func test_percent_discount() {
        theCart.addItem(product: rice)
        teller.addSpecialOffer(offerType: SpecialOfferType.TenPercentDiscount, product: rice, argument: 10.0)
        let receipt = teller.checksOutArticlesFrom(theCart: theCart)

        let result = ReceiptPrinter(columns: 40).printReceipt(receipt: receipt)
        
        let expected =
            """
            rice                                2.99
            10.0% off(rice)                    -0.30

            Total:                              2.69
            """
        XCTAssertEqual(expected, result)
    }

    func test_xForY_discount() {
        theCart.addItem(product: cherryTomatoes)
        theCart.addItem(product: cherryTomatoes)
        teller.addSpecialOffer(offerType: SpecialOfferType.TwoForAmount, product: cherryTomatoes, argument: 0.99)
        let receipt = teller.checksOutArticlesFrom(theCart: theCart)

        let result = ReceiptPrinter(columns: 40).printReceipt(receipt: receipt)
        
        let expected =
            """
            cherry tomato box                   0.69
            cherry tomato box                   0.69
            2 for 0.99(cherry tomato box)      -0.39

            Total:                              0.99
            """
        XCTAssertEqual(expected, result)
    }

    func test_xForY_discount_with_insufficient_in_basket() {
        theCart.addItem(product: cherryTomatoes)
        teller.addSpecialOffer(offerType: SpecialOfferType.TwoForAmount, product: cherryTomatoes, argument: 0.99)
        let receipt = teller.checksOutArticlesFrom(theCart: theCart)

        let result = ReceiptPrinter(columns: 40).printReceipt(receipt: receipt)
        
        let expected =
            """
            cherry tomato box                   0.69

            Total:                              0.69
            """
        XCTAssertEqual(expected, result)
    }

    func test_FiveForY_discount() {
        theCart.addItemQuantity(product: apples, quantity: 5)
        teller.addSpecialOffer(offerType: SpecialOfferType.FiveForAmount, product: apples,argument: 6.99)
        let receipt = teller.checksOutArticlesFrom(theCart: theCart)

        let result = ReceiptPrinter(columns: 40).printReceipt(receipt: receipt)
        
        let expected =
            """
            apples                              9.95
              1.99 * 5.000
            5 for 6.99(apples)                 -2.96

            Total:                              6.99
            """
        XCTAssertEqual(expected, result)
    }

    func test_FiveForY_discount_withSix() {
        theCart.addItemQuantity(product: apples, quantity: 6)
        teller.addSpecialOffer(offerType: SpecialOfferType.FiveForAmount, product: apples,argument: 5.99)
        let receipt = teller.checksOutArticlesFrom(theCart: theCart)

        let result = ReceiptPrinter(columns: 40).printReceipt(receipt: receipt)
        
        let expected =
            """
            apples                             11.94
              1.99 * 6.000
            5 for 5.99(apples)                 -3.96

            Total:                              7.98
            """
        XCTAssertEqual(expected, result)
    }

    func test_FiveForY_discount_withSixteen() {
        theCart.addItemQuantity(product: apples, quantity: 16)
        teller.addSpecialOffer(offerType: SpecialOfferType.FiveForAmount, product: apples,argument: 7.99)
        let receipt = teller.checksOutArticlesFrom(theCart: theCart)

        let result = ReceiptPrinter(columns: 40).printReceipt(receipt: receipt)
        
        let expected =
            """
            apples                             31.84
              1.99 * 16.000
            5 for 7.99(apples)                 -5.88

            Total:                             25.96
            """
        XCTAssertEqual(expected, result)
    }

    func test_FiveForY_discount_withFour() {
        theCart.addItemQuantity(product: apples, quantity: 4)
        teller.addSpecialOffer(offerType: SpecialOfferType.FiveForAmount, product: apples,argument: 8.99)
        let receipt = teller.checksOutArticlesFrom(theCart: theCart)

        let result = ReceiptPrinter(columns: 40).printReceipt(receipt: receipt)
        
        let expected =
            """
            apples                              7.96
              1.99 * 4.000

            Total:                              7.96
            """
        XCTAssertEqual(expected, result)
    }
}
