public class FakeCatalog: SupermarketCatalog {
    private var products = [String: Product]()
    private var prices = [String: Double]()

    public func addProduct(product: Product, price: Double) {
        self.products.append(product.name, product)
        self.prices.append(product.name, price)
    }

    public func getUnitPrice(p: Product) -> Double {
        return this.prices.get(p.getName());
    }
}
