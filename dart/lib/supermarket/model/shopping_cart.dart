import 'package:supermarket_receipt/supermarket/model/discount.dart';
import 'package:supermarket_receipt/supermarket/model/offer.dart';
import 'package:supermarket_receipt/supermarket/model/product.dart';
import 'package:supermarket_receipt/supermarket/model/product_quantity.dart';
import 'package:supermarket_receipt/supermarket/model/receipt.dart';
import 'package:supermarket_receipt/supermarket/model/special_offer_type.dart';
import 'package:supermarket_receipt/supermarket/model/supermarket_catalog.dart';

class ShoppingCart {

  final List<ProductQuantity> _items = [];
  final Map<Product, double> _productQuantities = {};

  void addItem(Product product) {
    addItemQuantity(product, 1.0);
  }

  void addItemQuantity(Product product, double quantity) {
    _items.add(ProductQuantity(product, quantity));
    if (_productQuantities.containsKey(product)) {
      _productQuantities[product] = _productQuantities[product]! + quantity;
    } else {
      _productQuantities[product] = quantity;
    }
  }

  List<ProductQuantity> getItems() {
    return _items;
  }

  Map<Product, double> getProductQuantities() {
    return _productQuantities;
  }

  void handleOffers(Receipt receipt, Map<Product, Offer> offers, SupermarketCatalog catalog) {
    for (Product p in _productQuantities.keys) {
      double quantity = _productQuantities[p]!;
      if (offers.containsKey(p)) {
        Offer offer = offers[p]!;
        double unitPrice = catalog.getUnitPrice(p);
        int quantityAsInt = quantity.toInt();
        Discount? discount = null;
        int x = 1;
        if (offer.offerType == SpecialOfferType.THREE_FOR_TWO) {
          x = 3;
        }
        else if (offer.offerType == SpecialOfferType.TWO_FOR_AMOUNT) {
          x = 2;
          if (quantityAsInt >= 2) {
            double total = offer.argument * (quantityAsInt ~/ x) + quantityAsInt % 2 * unitPrice;
            double discountN = unitPrice * quantity - total;
            discount = Discount(p, "2 for " + offer.argument.toString(), -discountN);
          }
        }
        else if (offer.offerType == SpecialOfferType.FIVE_FOR_AMOUNT) {
          x = 5;
        }
        int numberOfXs = quantityAsInt ~/ x;
        if (offer.offerType == SpecialOfferType.THREE_FOR_TWO && quantityAsInt > 2) {
          double discountAmount = quantity * unitPrice - ((numberOfXs * 2 * unitPrice) + quantityAsInt % 3 * unitPrice);
          discount = Discount(p, "3 for 2", -discountAmount);
        }
        if (offer.offerType == SpecialOfferType.TEN_PERCENT_DISCOUNT) {
          discount = Discount(p, offer.argument.toString() + "% off", -quantity * unitPrice * offer.argument / 100.0);
        }
        if (offer.offerType == SpecialOfferType.FIVE_FOR_AMOUNT && quantityAsInt >= 5) {
          double discountTotal = unitPrice * quantity - (offer.argument * numberOfXs + quantityAsInt % 5 * unitPrice);
          discount = Discount(p, x.toString() + " for " + offer.argument.toString(), -discountTotal);
        }
        if (discount != null) receipt.addDiscount(discount);
      }
    }
  }
}