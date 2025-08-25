import 'package:supermarket_receipt/supermarket/model/product.dart';

abstract class SupermarketCatalog {

  void addProduct(Product product, double price);

  double getUnitPrice(Product product);
}