package dojo.supermarket;

import dojo.supermarket.model.*;
import org.approvaltests.Approvals;
import org.approvaltests.reporters.JunitReporter;
import org.approvaltests.reporters.UseReporter;
import org.junit.jupiter.api.Test;

class ReceiptPrinterTest {

    private final Product toothbrush = new Product("toothbrush", ProductUnit.Each);
    private final Product apples = new Product("apples", ProductUnit.Kilo);
    private final Receipt receipt = new Receipt();

    @Test
    void oneLineItem() {
        receipt.addProduct(toothbrush, 1, 0.99, 0.99);
        Approvals.verify(new ReceiptPrinter(40).printReceipt(receipt));
    }

    @Test
    void quantityTwo() {
        receipt.addProduct(toothbrush, 2, 0.99,0.99 * 2);
        Approvals.verify(new ReceiptPrinter(40).printReceipt(receipt));
    }

    @Test
    void looseWeight() {
        apples.setQuantityType(" KG");
        receipt.addProduct(apples, 2.3, 1.99,1.99 * 2.3);
        Approvals.verify(new ReceiptPrinter(40).printReceipt(receipt));
    }

    @Test
    void total() {

        receipt.addProduct(toothbrush, 1, 0.99, 2*0.99);
        receipt.addProduct(apples, 0.75, 1.99, 1.99*0.75);
        Approvals.verify(new ReceiptPrinter(40).printReceipt(receipt));
    }

    @Test
    void discounts() {
        receipt.addDiscount(new Discount(apples, "3 for 2", -0.99));
        Approvals.verify(new ReceiptPrinter(40).printReceipt(receipt));
    }

    @Test
    void printWholeReceipt() {
        receipt.addProduct(toothbrush, 1, 0.99, 0.99);
        receipt.addProduct(toothbrush, 2, 0.99, 2*0.99);
        receipt.addProduct(apples, 0.75, 1.99, 1.99*0.75);
        receipt.addDiscount(new Discount(toothbrush, "3 for 2", -0.99));
        Approvals.verify(new ReceiptPrinter(40).printReceipt(receipt));
    }

}
