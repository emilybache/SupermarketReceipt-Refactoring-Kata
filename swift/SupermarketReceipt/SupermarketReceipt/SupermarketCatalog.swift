public protocol SupermarketCatalog {
    func addProduct(product: Product, price: Double)
    func getUnitPrice(product: Product) -> Double
}
