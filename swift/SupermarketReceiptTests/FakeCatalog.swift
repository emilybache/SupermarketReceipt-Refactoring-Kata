@testable import SupermarketReceipt

public class FakeCatalog: SupermarketCatalog {
    private var products = [String: Product]()
    private var prices = [String: Double]()

    public func addProduct(product: Product, price: Double) {
        self.products[product.name] = product
        self.prices[product.name] = price
    }

    public func getUnitPrice(product: Product) -> Double {
        return self.prices[product.name] ?? 0;
    }
}
