package supermarket.model

interface SupermarketCatalog {
    fun addProduct(product: Product, price: Double)

    fun getUnitPrice(product: Product): Double
}
