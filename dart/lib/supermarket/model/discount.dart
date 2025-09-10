import 'package:supermarket_receipt/supermarket/model/product.dart';

class Discount {

  final Product _product;
  final String _description;
  final double _discountAmount;

  Discount(this._product, this._description, this._discountAmount);

  String getDescription() => _description;

  double getDiscountAmount() => _discountAmount;

  Product getProduct() => _product;
}