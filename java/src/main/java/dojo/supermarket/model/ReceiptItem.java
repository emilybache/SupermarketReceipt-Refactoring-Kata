package dojo.supermarket.model;

public record ReceiptItem(Product product, double quantity, double price, double totalPrice) {
}
