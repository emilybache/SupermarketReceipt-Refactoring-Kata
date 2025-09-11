import 'package:supermarket_receipt/supermarket/model/offer.dart';
import 'package:supermarket_receipt/supermarket/model/product.dart';
import 'package:supermarket_receipt/supermarket/model/product_quantity.dart';
import 'package:supermarket_receipt/supermarket/model/receipt.dart';
import 'package:supermarket_receipt/supermarket/model/shopping_cart.dart';
import 'package:supermarket_receipt/supermarket/model/special_offer_type.dart';
import 'package:supermarket_receipt/supermarket/model/supermarket_catalog.dart';

class Teller {

  final SupermarketCatalog _catalog;
  final Map<Product, Offer> _offers = {};

  Teller(this._catalog);

  void addSpecialOffer(SpecialOfferType offerType, Product product, double argument) {
    _offers[product] = Offer(offerType, product, argument);
  }

  Receipt checksOutArticlesFrom(ShoppingCart theCart) {
    Receipt receipt = Receipt();
    List<ProductQuantity> productQuantities = theCart.getItems();
    for (var pq in productQuantities) {
      Product p = pq.getProduct();
      double quantity = pq.getQuantity();
      double unitPrice = _catalog.getUnitPrice(p);
      double price = quantity * unitPrice;
      receipt.addProduct(p, quantity, unitPrice, price);
    }
    theCart.handleOffers(receipt, _offers, _catalog);

    return receipt;
  }
}
