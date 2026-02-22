package dojo.supermarket.model;

import java.util.Objects;

public record ReceiptItem(Product product, double quantity, double price, double totalPrice) {
}
