import 'package:supermarket_receipt/supermarket/model/product.dart';
import 'package:supermarket_receipt/supermarket/model/product_unit.dart';
import 'package:supermarket_receipt/supermarket/model/receipt.dart';
import 'package:supermarket_receipt/supermarket/model/receipt_item.dart';
import 'package:supermarket_receipt/supermarket/model/shopping_cart.dart';
import 'package:supermarket_receipt/supermarket/model/special_offer_type.dart';
import 'package:supermarket_receipt/supermarket/model/supermarket_catalog.dart';
import 'package:supermarket_receipt/supermarket/model/teller.dart';
import 'package:supermarket_receipt/supermarket/receipt_printer.dart';
import 'package:test/test.dart';

import '../fake_katalog.dart';

void main() {
  group('SupermarketAllTests', () {
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
      cart.addItemQuantity(toothbrush, 1.0);

      Receipt receipt = teller.checksOutArticlesFrom(cart);

      ReceiptItem receiptItem = receipt.getItems()[0];
      expect(receiptItem.getProduct(), apples);
      expect(receiptItem.getPrice(), 1.99);
      expect(receiptItem.getTotalPrice(), 2.5 * 1.99);
      expect(receiptItem.getQuantity(), 2.5);

      receiptItem = receipt.getItems()[1];
      expect(receiptItem.getProduct(), toothbrush);
      expect(receiptItem.getQuantity(), 1.0);
      expect(receiptItem.getPrice(), 0.99);
      expect(receiptItem.getTotalPrice(), 0.99);

      double totalPrice = 2.5 * 1.99 + (0.99 - 0.99 * 0.1);
      expect(receipt.getTotalPrice(), closeTo(totalPrice, 0.01));
      expect(receipt.getDiscounts(), isNotEmpty);
      expect(receipt.getItems().length, 2);

      printReceipt('tenPercentDiscount', receipt);
    });

    test('threeForTwo', () {
      SupermarketCatalog catalog = FakeCatalog();
      Product apples = Product('apples', ProductUnit.KILO);
      catalog.addProduct(apples, 1.99);
      Product chocolate = Product('chocolate', ProductUnit.EACH);
      catalog.addProduct(chocolate, 0.99);
      Product toothbrush = Product('toothbrush', ProductUnit.EACH);
      catalog.addProduct(toothbrush, 0.99);

      Teller teller = Teller(catalog);
      teller.addSpecialOffer(SpecialOfferType.THREE_FOR_TWO, chocolate, 0.0);
      teller.addSpecialOffer(SpecialOfferType.THREE_FOR_TWO, toothbrush, 0.0);

      ShoppingCart cart = ShoppingCart();
      cart.addItemQuantity(apples, 2.5);
      cart.addItemQuantity(chocolate, 3.0);
      cart.addItemQuantity(toothbrush, 1.0);

      Receipt receipt = teller.checksOutArticlesFrom(cart);

      ReceiptItem receiptItem = receipt.getItems()[0];
      expect(receiptItem.getProduct(), apples);
      expect(receiptItem.getPrice(), 1.99);
      expect(receiptItem.getTotalPrice(), 2.5 * 1.99);
      expect(receiptItem.getQuantity(), 2.5);

      receiptItem = receipt.getItems()[1];
      expect(receiptItem.getProduct(), chocolate);
      expect(receiptItem.getQuantity(), 3.0);
      expect(receiptItem.getPrice(), 0.99);
      expect(receiptItem.getTotalPrice(), 0.99 * 3.0);

      receiptItem = receipt.getItems()[2];
      expect(receiptItem.getProduct(), toothbrush);
      expect(receiptItem.getQuantity(), 1.0);
      expect(receiptItem.getPrice(), 0.99);
      expect(receiptItem.getTotalPrice(), 0.99);

      expect(receipt.getTotalPrice(), closeTo(0.99 * 2.0 + 2.5 * 1.99 + 0.99, 0.01));
      expect(receipt.getDiscounts(), isNotEmpty);
      expect(receipt.getItems().length, 3);

      printReceipt('threeForTwo', receipt);
    });

    test('twoForAmount', () {
      SupermarketCatalog catalog = FakeCatalog();
      Product apples = Product('apples', ProductUnit.KILO);
      catalog.addProduct(apples, 1.99);
      Product cherryTomatoesBox = Product('cherry tomatoes', ProductUnit.EACH);
      catalog.addProduct(cherryTomatoesBox, 0.69);

      Teller teller = Teller(catalog);
      teller.addSpecialOffer(SpecialOfferType.TWO_FOR_AMOUNT, cherryTomatoesBox, 0.99);

      ShoppingCart cart = ShoppingCart();
      cart.addItemQuantity(apples, 2.5);
      cart.addItemQuantity(cherryTomatoesBox, 3.0);

      Receipt receipt = teller.checksOutArticlesFrom(cart);

      ReceiptItem receiptItem = receipt.getItems()[0];
      expect(receiptItem.getProduct(), apples);
      expect(receiptItem.getPrice(), 1.99);
      expect(receiptItem.getTotalPrice(), 2.5 * 1.99);
      expect(receiptItem.getQuantity(), 2.5);

      receiptItem = receipt.getItems()[1];
      expect(receiptItem.getProduct(), cherryTomatoesBox);
      expect(receiptItem.getQuantity(), 3.0);
      expect(receiptItem.getPrice(), 0.69);
      expect(receiptItem.getTotalPrice(), 0.69 * 3);

      expect(receipt.getTotalPrice(), closeTo(4.975 + 1.68, 0.01));
      expect(receipt.getDiscounts(), isNotEmpty);
      expect(receipt.getItems().length, 2);

      printReceipt('twoForAmount', receipt);
    });

    test('fiveForAmount', () {
      SupermarketCatalog catalog = FakeCatalog();
      Product apples = Product('apples', ProductUnit.KILO);
      catalog.addProduct(apples, 1.99);
      Product toothpaste = Product('toothpaste', ProductUnit.EACH);
      catalog.addProduct(toothpaste, 1.79);
      Product toothbrush = Product('toothbrush', ProductUnit.EACH);
      catalog.addProduct(toothbrush, 0.99);

      Teller teller = Teller(catalog);
      teller.addSpecialOffer(SpecialOfferType.FIVE_FOR_AMOUNT, toothpaste, 7.49);
      teller.addSpecialOffer(SpecialOfferType.FIVE_FOR_AMOUNT, toothbrush, 4.49);

      double toothpasteQuantity = 6.0;
      double expectedToothpastePrice = 1.79 + 7.49;

      ShoppingCart cart = ShoppingCart();
      cart.addItemQuantity(apples, 2.5);
      cart.addItemQuantity(toothpaste, toothpasteQuantity);
      cart.addItemQuantity(toothbrush, 1.0);

      Receipt receipt = teller.checksOutArticlesFrom(cart);

      ReceiptItem receiptItem = receipt.getItems()[0];
      expect(receiptItem.getProduct(), apples);
      expect(receiptItem.getPrice(), 1.99);
      expect(receiptItem.getTotalPrice(), 2.5 * 1.99);
      expect(receiptItem.getQuantity(), 2.5);

      receiptItem = receipt.getItems()[1];
      expect(receiptItem.getProduct(), toothpaste);
      expect(receiptItem.getQuantity(), toothpasteQuantity);
      expect(receiptItem.getPrice(), 1.79);
      expect(receiptItem.getTotalPrice(), 1.79 * toothpasteQuantity);

      receiptItem = receipt.getItems()[2];
      expect(receiptItem.getProduct(), toothbrush);
      expect(receiptItem.getQuantity(), 1.0);
      expect(receiptItem.getPrice(), 0.99);
      expect(receiptItem.getTotalPrice(), 0.99);

      expect(receipt.getTotalPrice(), closeTo(4.975 + expectedToothpastePrice + 0.99, 0.01));
      expect(receipt.getDiscounts(), isNotEmpty);
      expect(receipt.getItems().length, 3);

      printReceipt('fiveForAmount', receipt);
    });

    test('noDiscount', () {
      SupermarketCatalog catalog = FakeCatalog();
      Product toothbrush = Product('toothbrush', ProductUnit.EACH);
      catalog.addProduct(toothbrush, 0.99);
      Product apples = Product('apples', ProductUnit.KILO);
      catalog.addProduct(apples, 1.99);

      Teller teller = Teller(catalog);
      teller.addSpecialOffer(SpecialOfferType.THREE_FOR_TWO, toothbrush, 0.0);

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

      printReceipt('noDiscount', receipt);
    });
  });
}

void printReceipt(String name, Receipt receipt) {
    ReceiptPrinter receiptPrinter = ReceiptPrinter();
    String receiptPresentation = receiptPrinter.printReceipt(receipt);
    print(name);
    print(receiptPresentation);
}