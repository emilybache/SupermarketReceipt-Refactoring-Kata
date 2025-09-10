import 'package:supermarket_receipt/supermarket/model/product.dart';

class ProductQuantity {

  final Product _product;
  final double _quantity;

  ProductQuantity(this._product, this._quantity);

  Product getProduct() => _product;

  double getQuantity() => _quantity;
}