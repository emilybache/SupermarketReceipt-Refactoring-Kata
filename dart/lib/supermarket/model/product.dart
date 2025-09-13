import 'package:supermarket_receipt/supermarket/model/product_unit.dart';

class Product {

  final String _name;
  final ProductUnit _unit;

  Product(this._name, this._unit);

  String getName() => _name;

  ProductUnit getUnit() => _unit;

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is Product &&
          runtimeType == other.runtimeType &&
          _name == other._name &&
          _unit == other._unit;

  @override
  int get hashCode => _name.hashCode ^ _unit.hashCode;
}