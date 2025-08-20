import 'package:supermarket_receipt/supermarket/model/discount.dart';
import 'package:supermarket_receipt/supermarket/model/product.dart';
import 'package:supermarket_receipt/supermarket/model/receipt_item.dart';

class Receipt {
  
  final List<ReceiptItem> _items = [];
  final List<Discount> _discounts = [];

  void addProduct(Product p, double quantity, double price, double totalPrice) {
    _items.add(ReceiptItem(p, quantity, price, totalPrice));
  }

  void addDiscount(Discount discount) {
    _discounts.add(discount);
  }

  double getTotalPrice() {
    double total = 0.0;
    for (var item in _items) {
      total += item.getTotalPrice();
    }
    for (var discount in _discounts) {
      total += discount.getDiscountAmount();
    }
    return total;
  }

  List<ReceiptItem> getItems() => _items;

  List<Discount> getDiscounts() => _discounts;
}