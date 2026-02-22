package dojo.supermarket.model;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

public class Receipt {

    private final List<ReceiptItem> items = new ArrayList<>();
    private final List<Discount> discounts = new ArrayList<>();
    private Date date;

    public Receipt() {
        this.date = new Date();
    }

    public Double getTotalPrice() {
        return this.items.stream().mapToDouble(ReceiptItem::totalPrice).sum()
                + this.discounts.stream().mapToDouble(Discount::discountAmount).sum();
    }

    public void addProduct(Product p, double quantity, double price, double totalPrice) {
        this.items.add(new ReceiptItem(p, quantity, price, totalPrice));
    }

    public List<ReceiptItem> getItems() {
        return new ArrayList<>(this.items);
    }

    public void addDiscount(Discount discount) {
        this.discounts.add(discount);
    }

    public List<Discount> getDiscounts() {
        return discounts;
    }

    public Date getDate() {
        return date;
    }
}
