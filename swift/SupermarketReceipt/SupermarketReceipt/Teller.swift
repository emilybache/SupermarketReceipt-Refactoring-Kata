public class Teller {

    private let catalog: SupermarketCatalog
    private var offers = [Product: Offer]()

    public init(catalog: SupermarketCatalog) {
        self.catalog = catalog
    }

    public func addSpecialOffer(offerType: SpecialOfferType, product: Product, argument: Double) {
        self.offers[product] = Offer(offerType: offerType, product: product, argument: argument)
    }

    public func checksOutArticlesFrom(theCart: ShoppingCart) -> Receipt {
        var receipt = Receipt()
        var productQuantities = theCart.items
        for pq in productQuantities {
            var p = pq.product
            var quantity = pq.quantity
            var unitPrice = self.catalog.getUnitPrice(product: p)
            var price = quantity * unitPrice
            var priceTo3dp = round(100 * price) / 100
            receipt.addProduct(p: p, quantity: quantity, price: unitPrice, totalPrice: priceTo3dp)
        }
        theCart.handleOffers(receipt: receipt, offers: self.offers, catalog: self.catalog)

        return receipt
    }

}
