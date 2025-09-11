import 'package:supermarket_receipt/supermarket/model/product.dart';
import 'package:supermarket_receipt/supermarket/model/special_offer_type.dart';


class Offer {

  final SpecialOfferType offerType;
  final Product _product;
  final double argument;

  Offer(this.offerType, this._product, this.argument);

  Product getProduct() => _product;
}