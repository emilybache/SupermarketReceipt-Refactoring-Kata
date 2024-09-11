from typing import List
from models.product import ProductUnit
from models.discount import Discount
from models.receipt import Receipt, ReceiptItem

class ReceiptPrinter:

    def __init__(self, columns: int = 40) -> None:
        self.columns = columns
  
    def print_receipt(self, receipt: Receipt) -> str:
        lines: List[str] = [] # replace string concat with list[str]

        for item in receipt.items:
            receipt_item = self.print_receipt_item(item)
            lines.append(receipt_item)

        for discount in receipt.discounts:
            discount_presentation = self.print_discount(discount)
            lines.append(discount_presentation)

        lines.append("\n")
        lines.append(self.present_total(receipt))

        return "".join(lines)

    def print_receipt_item(self, item: ReceiptItem) -> str:
        total_price_printed = self.print_price(item.total_price)
        name = item.product.name
        line = self.format_line_with_whitespace(name, total_price_printed)
        if item.quantity != 1:
            line += f"  {self.print_price(item.price)} * {self.print_quantity(item)}\n"
        return line

    def format_line_with_whitespace(self, name: str, value: str) -> str:
        # validate no overflow in the columns size
        if len(name) + len(value) > self.columns:
            name = name[:self.columns - len(value) - 1] + "…"
        whitespace_size = self.columns - len(name) - len(value)
        # line = name + " " * whitespace_size + value + "\n"
        line = name + " ".ljust(whitespace_size) + value + "\n"
        return line

    def print_price(self, price: int) -> str:
        return "%.2f" % price

    def print_quantity(self, item: ReceiptItem) -> str:
        if ProductUnit.EACH == item.product.unit:
            return str(item.quantity)
        else:
            return '%.3f' % item.quantity

    def print_discount(self, discount: Discount) -> str:
        name = f"{discount.description} ({discount.product.name})"
        value = self.print_price(discount.discount_amount)
        return self.format_line_with_whitespace(name, value)

    def present_total(self, receipt: Receipt) -> str:
        name = "Total: "
        value = self.print_price(receipt.total_price())
        return self.format_line_with_whitespace(name, value)
