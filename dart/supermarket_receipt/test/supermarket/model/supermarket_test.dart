import 'package:supermarket_receipt/supermarket/model/product.dart';
import 'package:supermarket_receipt/supermarket/model/product_unit.dart';
import 'package:supermarket_receipt/supermarket/model/receipt.dart';
import 'package:supermarket_receipt/supermarket/model/receipt_item.dart';
import 'package:supermarket_receipt/supermarket/model/shopping_cart.dart';
import 'package:supermarket_receipt/supermarket/model/special_offer_type.dart';
import 'package:supermarket_receipt/supermarket/model/supermarket_catalog.dart';
import 'package:supermarket_receipt/supermarket/model/teller.dart';
import 'package:test/test.dart';

import '../fake_katalog.dart';

void main() {
  group('SupermarketTest', () {
    test('tenPercentDiscount', () {
      SupermarketCatalog catalog = FakeCatalog();
      Product toothbrush = Product('toothbrush', ProductUnit.EACH);
      catalog.addProduct(toothbrush, 0.99);
      Product apples = Product('apples', ProductUnit.KILO);
      catalog.addProduct(apples, 1.99);

      Teller teller = Teller(catalog);
      teller.addSpecialOffer(SpecialOfferType.TEN_PERCENT_DISCOUNT, toothbrush, 10.0);

      ShoppingCart cart = ShoppingCart();
      cart.addItemQuantity(apples, 2.5);

      Receipt receipt = teller.checksOutArticlesFrom(cart);

      expect(receipt.getTotalPrice(), closeTo(4.975, 0.01));
      expect(receipt.getDiscounts(), isEmpty);
      expect(receipt.getItems().length, 1);
      ReceiptItem receiptItem = receipt.getItems()[0];
      expect(receiptItem.getProduct(), apples);
      expect(receiptItem.getPrice(), 1.99);
      expect(receiptItem.getTotalPrice(), 2.5 * 1.99);
      expect(receiptItem.getQuantity(), 2.5);
    });
  });
}