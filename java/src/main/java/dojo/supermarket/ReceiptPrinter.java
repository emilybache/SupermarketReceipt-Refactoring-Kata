package dojo.supermarket;

import dojo.supermarket.model.*;

import java.util.Locale;

public class ReceiptPrinter {

    private final int columns;

    public ReceiptPrinter() {
        this(40);
    }

    public ReceiptPrinter(int columns) {
        this.columns = columns;
    }

    public String printReceipt(Receipt receipt) {
        StringBuilder result = new StringBuilder();
        for (ReceiptItem item : receipt.getItems()) {
            String price = String.format(Locale.UK, "%.2f", item.getTotalPrice());
            String quantity = presentQuantity(item);
            String name = item.getProduct().getName();
            String unitPrice = String.format(Locale.UK, "%.2f", item.getPrice());

            int whitespaceSize = this.columns - name.length() - price.length();
            String line = name + getWhitespace(whitespaceSize) + price + "\n";

            if (item.getQuantity() != 1) {
                line += "  " + unitPrice + " * " + quantity + "\n";
            }
            result.append(line);
        }
        for (Discount discount : receipt.getDiscounts()) {
            String productPresentation = discount.getProduct().getName();
            String pricePresentation = String.format(Locale.UK, "%.2f", discount.getDiscountAmount());
            String description = discount.getDescription();
            result.append(description);
            result.append("(");
            result.append(productPresentation);
            result.append(")");
            result.append(getWhitespace(this.columns - 3 - productPresentation.length() - description.length() - pricePresentation.length()));
            result.append("-");
            result.append(pricePresentation);
            result.append("\n");
        }
        result.append("\n");
        String pricePresentation = String.format(Locale.UK, "%.2f", (double) receipt.getTotalPrice());
        String total = "Total: ";
        String whitespace = getWhitespace(this.columns - total.length() - pricePresentation.length());
        result.append(total).append(whitespace).append(pricePresentation);
        return result.toString();
    }

    private static String presentQuantity(ReceiptItem item) {
        return ProductUnit.Each.equals(item.getProduct().getUnit())
                ? String.format("%x", (int)item.getQuantity())
                : String.format(Locale.UK, "%.3f", item.getQuantity());
    }

    private static String getWhitespace(int whitespaceSize) {
        StringBuilder whitespace = new StringBuilder();
        for (int i = 0; i < whitespaceSize; i++) {
            whitespace.append(" ");
        }
        return whitespace.toString();
    }
}
