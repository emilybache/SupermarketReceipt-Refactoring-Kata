from python.domain.model_objects import ProductUnit


class ReceiptPrinter:
    """
    Service responsible for formatting and printing receipts.

    This printer handles the text-based receipt format.
    Future implementations could include HTML, PDF, or other formats.
    """

    def __init__(self, columns=40):
        self.columns = columns

    def print_receipt(self, receipt):
        """
        Generate a text representation of the receipt.

        Args:
            receipt: Receipt object containing items and discounts

        Returns:
            str: Formatted receipt text
        """
        result = ""

        # Add all items
        for item in receipt.items:
            receipt_item = self._format_receipt_item(item)
            result += receipt_item

        # Add all discounts
        for discount in receipt.discounts:
            discount_line = self._format_discount(discount)
            result += discount_line

        # Add total
        result += "\n"
        result += self._format_total(receipt)

        return str(result)

    def _format_receipt_item(self, item):
        """Format a single receipt item"""
        total_price_printed = self._format_price(item.total_price)
        name = item.product.name
        line = self._format_line_with_whitespace(name, total_price_printed)

        if item.quantity != 1:
            quantity_info = f"  {self._format_price(item.price)} * {self._format_quantity(item)}\n"
            line += quantity_info

        return line

    def _format_line_with_whitespace(self, name, value):
        """Format a line with proper spacing between name and value"""
        line = name
        whitespace_size = self.columns - len(name) - len(value)

        # Ensure minimum spacing
        if whitespace_size < 1:
            whitespace_size = 1

        line += " " * whitespace_size
        line += value
        line += "\n"
        return line

    def _format_price(self, price):
        """Format a price value"""
        return "%.2f" % price

    def _format_quantity(self, item):
        """Format quantity based on product unit type"""
        if ProductUnit.EACH == item.product.unit:
            return str(int(item.quantity)) if item.quantity == int(item.quantity) else str(item.quantity)
        else:
            return '%.3f' % item.quantity

    def _format_discount(self, discount):
        """Format a discount line"""
        name = f"{discount.description} ({discount.product.name})"
        value = self._format_price(discount.discount_amount)
        return self._format_line_with_whitespace(name, value)

    def _format_total(self, receipt):
        """Format the total line"""
        name = "Total: "
        value = self._format_price(receipt.total_price())
        return self._format_line_with_whitespace(name, value)
