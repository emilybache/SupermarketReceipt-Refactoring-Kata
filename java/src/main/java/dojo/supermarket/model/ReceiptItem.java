package dojo.supermarket.model;

public record ReceiptItem(Product product, double price, double totalPrice, double quantity) {
}
