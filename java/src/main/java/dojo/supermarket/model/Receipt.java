package dojo.supermarket.model;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Receipt {

    private final List<ReceiptItem> items = new ArrayList<>();
    private final List<Discount> discounts = new ArrayList<>();

    public double getTotalPrice() {
        return items.stream().mapToDouble(ReceiptItem::totalPrice).sum() //
                + discounts.stream().mapToDouble(Discount::discountAmount).sum();
    }

    public void addProduct(Product p, double quantity, double price, double totalPrice) {
        items.add(new ReceiptItem(p, quantity, price, totalPrice));
    }

    public List<ReceiptItem> getItems() {
        return Collections.unmodifiableList(items);
    }

    public void addDiscount(Discount discount) {
        discounts.add(discount);
    }

    public List<Discount> getDiscounts() {
        return discounts;
    }
}
