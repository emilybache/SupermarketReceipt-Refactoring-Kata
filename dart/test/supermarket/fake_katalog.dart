import 'package:supermarket_receipt/supermarket/model/product.dart';
import 'package:supermarket_receipt/supermarket/model/supermarket_catalog.dart';

class FakeCatalog implements SupermarketCatalog {
  
  final Map<String, Product> products = {};
  final Map<String, double> prices = {};

  @override
  void addProduct(Product product, double price) {
    products[product.getName()] = product;
    prices[product.getName()] = price;
  }

  @override
  double getUnitPrice(Product product) {
    return prices[product.getName()] ?? 0;
  }
}