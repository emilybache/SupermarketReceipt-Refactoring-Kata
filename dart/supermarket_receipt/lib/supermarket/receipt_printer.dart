import 'package:supermarket_receipt/supermarket/model/product_unit.dart';
import 'package:supermarket_receipt/supermarket/model/receipt.dart';
import 'package:supermarket_receipt/supermarket/model/receipt_item.dart';
import 'package:supermarket_receipt/supermarket/model/discount.dart';

class ReceiptPrinter {
  
  final int _columns;

  ReceiptPrinter() : this._columns = 40;

  ReceiptPrinter.withColumns(this._columns);

  String printReceipt(Receipt receipt) {
    StringBuffer result = StringBuffer();
    for (ReceiptItem item in receipt.getItems()) {
      String receiptItem = presentReceiptItem(item);
      result.write(receiptItem);
    }
    for (Discount discount in receipt.getDiscounts()) {
      String discountPresentation = presentDiscount(discount);
      result.write(discountPresentation);
    }

    result.write('\n');
    result.write(presentTotal(receipt));
    return result.toString();
  }

  String presentReceiptItem(ReceiptItem item) {
    String totalPricePresentation = presentPrice(item.getTotalPrice());
    String name = item.getProduct().getName();

    String line = formatLineWithWhitespace(name, totalPricePresentation);

    if (item.getQuantity() != 1) {
      line += '  ' + presentPrice(item.getPrice()) + ' * ' + presentQuantity(item) + '\n';
    }
    return line;
  }

  String presentDiscount(Discount discount) {
    String name = discount.getDescription() + ' (' + discount.getProduct().getName() + ')';
    String value = presentPrice(discount.getDiscountAmount());

    return formatLineWithWhitespace(name, value);
  }

  String presentTotal(Receipt receipt) {
    String name = 'Total: ';
    String value = presentPrice(receipt.getTotalPrice());
    return formatLineWithWhitespace(name, value);
  }

  String formatLineWithWhitespace(String name, String value) {
    StringBuffer line = StringBuffer();
    line.write(name);
    int whitespaceSize = _columns - name.length - value.length;
    for (int i = 0; i < whitespaceSize; i++) {
      line.write(' ');
    }
    line.write(value);
    line.write('\n');
    return line.toString();
  }

  String presentPrice(double price) {
    return price.toStringAsFixed(2);
  }

  String presentQuantity(ReceiptItem item) {
    if (item.getProduct().getUnit() == ProductUnit.EACH) {
      return item.getQuantity().toInt().toString();
    } else {
      return item.getQuantity().toStringAsFixed(3);
    }
  }
}