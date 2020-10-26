import unittest

from approvaltests import verify

from model_objects import Product, SpecialOfferType, ProductUnit, Discount
from receipt import Receipt
from receipt_printer import ReceiptPrinter



class ReceiptPrinterTest(unittest.TestCase):
    def setUp(self):
        self.toothbrush = Product("toothbrush", ProductUnit.EACH)
        self.apples = Product("apples", ProductUnit.KILO)
        self.receipt = Receipt()

    def test_one_line_item(self):
        self.receipt.add_product(self.toothbrush, 1, 0.99, 0.99)
        verify(ReceiptPrinter().print_receipt(self.receipt))

    def test_quantity_two(self):
        self.receipt.add_product(self.toothbrush, 2, 0.99, 0.99 * 2)
        verify(ReceiptPrinter().print_receipt(self.receipt))

    def test_loose_weight(self):
        self.receipt.add_product(self.apples, 2.3, 1.99, 1.99 * 2.3)
        verify(ReceiptPrinter().print_receipt(self.receipt))

    def test_total(self):
        self.receipt.add_product(self.toothbrush, 1, 0.99, 0.99 * 2)
        self.receipt.add_product(self.apples, 0.75, 1.99, 1.99 * 0.75)
        verify(ReceiptPrinter().print_receipt(self.receipt))

    def test_discounts(self):
        self.receipt.add_discount(Discount(self.apples, "3 for 2", -0.99))
        verify(ReceiptPrinter().print_receipt(self.receipt))

    def test_whole_receipt(self):
        self.receipt.add_product(self.toothbrush, 1, 0.99, 0.99)
        self.receipt.add_product(self.toothbrush, 2, 0.99, 0.99*2)
        self.receipt.add_product(self.apples, 0.75, 1.99, 1.99 * 0.75)
        self.receipt.add_discount(Discount(self.apples, "3 for 2", -0.99))
        verify(ReceiptPrinter().print_receipt(self.receipt))
