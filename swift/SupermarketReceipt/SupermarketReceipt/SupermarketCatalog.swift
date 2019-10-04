public protocol SupermarketCatalog {
    func addProduct(peoduct: Product, price: Double)
    func getUnitPrice(product: Product) -> Double
}
