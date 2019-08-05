from model_objects import ProductUnit


def whitespace(whitespace_size):
    space = ''
    for i in range(whitespace_size):
        space += ' '

    return space


def present_quantity(item):
    if ProductUnit.EACH == item.product.unit:
        return str(item.quantity)
    else:
        return '%.3f' % item.quantity


class ReceiptPrinter:

    def __init__(self, columns=40):
        self.columns = columns
  
    def print_receipt(self, receipt):
        result = ""
        for item in receipt.items:
            price = "%.2f" % item.total_price
            quantity = present_quantity(item)
            name = item.product.name
            unit_price = "%.2f" % item.price

            whitespace_size = self.columns - len(name) - len(price)
            line = name + whitespace(whitespace_size) + price + "\n"

            if item.quantity != 1:
                line += "  " + unit_price + " * " + quantity + "\n"

            result += line

        for discount in receipt.discounts:
            product_presentation = discount.product.name
            price_presentation = "%.2f" % discount.discount_amount
            description = discount.description
            result += description
            result += "("
            result += product_presentation
            result += ")"
            result += whitespace(self.columns - 3 - len(product_presentation) - len(description) - len(price_presentation))
            result += "-"
            result += price_presentation
            result += "\n"

        result += "\n"
        price_presentation = "%.2f" % receipt.total_price()
        total = "Total: "
        space = whitespace(self.columns - len(total) - len(price_presentation))
        result += total + space + price_presentation
        return str(result)
