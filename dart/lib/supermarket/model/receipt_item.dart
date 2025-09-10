import 'package:supermarket_receipt/supermarket/model/product.dart';

class ReceiptItem {

  final Product _product;
  final double _quantity;
  final double _price;
  final double _totalPrice;

  ReceiptItem(this._product, this._quantity, this._price, this._totalPrice);

  Product getProduct() => _product;

  double getQuantity() => _quantity;

  double getPrice() => _price;

  double getTotalPrice() => _totalPrice;

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is ReceiptItem &&
          runtimeType == other.runtimeType &&
          _product == other._product &&
          _quantity == other._quantity &&
          _price == other._price &&
          _totalPrice == other._totalPrice;

  @override
  int get hashCode =>
      _product.hashCode ^
      _quantity.hashCode ^
      _price.hashCode ^
      _totalPrice.hashCode;
}