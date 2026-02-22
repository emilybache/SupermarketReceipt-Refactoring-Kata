package dojo.supermarket;

import dojo.supermarket.model.*;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

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
        result.append(header(receipt));

        for (ReceiptItem item : receipt.getItems()) {
            String receiptItem = presentReceiptItem(item);
            result.append(receiptItem);
        }
        for (Discount discount : receipt.getDiscounts()) {
            String discountPresentation = presentDiscount(discount);
            result.append(discountPresentation);
        }

        result.append("\n");
        result.append(presentTotal(receipt));
        return result.toString();
    }

    private String header(Receipt receipt) {
        StringBuilder result = new StringBuilder();

        result.append(horizontalLine());

        TimeZone tz = TimeZone.getTimeZone("UTC");
        DateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm"); // Quoted "Z" to indicate UTC, no timezone offset
        df.setTimeZone(tz);
        String nowAsISO = df.format(receipt.getDate());
        result.append(formatLineWithWhitespace("Receipt date:", nowAsISO));

        result.append(horizontalLine());
        return result.toString();
    }

    private String horizontalLine() {
        StringBuilder result = new StringBuilder();
        for (int i = 0; i < columns; i++) {
            result.append("-");
        }
        result.append("\n");
        return result.toString();
    }

    private String presentReceiptItem(ReceiptItem item) {
        String totalPricePresentation = presentPrice(item.totalPrice());
        String name = item.product().name();

        String line = formatLineWithWhitespace(name, totalPricePresentation);

        if (item.quantity() != 1) {
            line += "  " + presentPrice(item.price()) + " * " + presentQuantity(item) + "\n";
        }
        return line;
    }

    private String presentDiscount(Discount discount) {
        String name = discount.description() + "(" + discount.product().name() + ")";
        String value = presentPrice(discount.discountAmount());

        return formatLineWithWhitespace(name, value);
    }

    private String presentTotal(Receipt receipt) {
        String name = "Total: ";
        String value = presentPrice(receipt.getTotalPrice());
        return formatLineWithWhitespace(name, value);
    }

    private String formatLineWithWhitespace(String name, String value) {
        StringBuilder line = new StringBuilder();
        line.append(name);
        int whitespaceSize = this.columns - name.length() - value.length();
        line.append(" ".repeat(Math.max(0, whitespaceSize)));
        line.append(value);
        line.append('\n');
        return line.toString();
    }

    private static String presentPrice(double price) {
        return String.format(Locale.UK, "%.2f", price);
    }

    private static String presentQuantity(ReceiptItem item) {
        return ProductUnit.EACH.equals(item.product().unit())
                ? String.format("%x%s", (int)item.quantity(), item.getQuantityType())
                : String.format(Locale.UK, "%.3f%s", item.quantity(), item.getQuantityType());
    }

}
