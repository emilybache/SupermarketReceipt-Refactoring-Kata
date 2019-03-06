package supermarket.model

data class ReceiptItem(val product: Product, val quantity: Double, val price: Double, val totalPrice: Double)
