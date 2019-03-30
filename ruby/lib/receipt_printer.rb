class ReceiptPrinter

  def initialize(columns = 40)
    @columns = columns
  end

  def print_receipt(receipt)
    result = ""
    for item in receipt.items do
      price = "%.2f" % item.total_price
      quantity = self.class.present_quantity(item)
      name = item.product.name
      unit_price = "%.2f" % item.price

      whitespace_size = @columns - name.size - price.size
      line = name + self.class.whitespace(whitespace_size) + price + "\n"

      if item.quantity != 1
        line += "  " + unit_price + " * " + quantity + "\n"
      end

      result.concat(line);
    end
    for discount in receipt.discounts do
      product_presentation = discount.product.name
      price_presentation = "%.2f" % discount.discount_amount
      description = discount.description
      result.concat(description)
      result.concat("(")
      result.concat(product_presentation)
      result.concat(")")
      result.concat(self.class.whitespace(@columns - 3 - product_presentation.size - description.size - price_presentation.size))
      result.concat("-");
      result.concat(price_presentation);
      result.concat("\n");
    end
    result.concat("\n")
    price_presentation = "%.2f" % receipt.total_price.to_f
    total = "Total: "
    whitespace = self.class.whitespace(@columns - total.size - price_presentation.size)
    result.concat(total, whitespace, price_presentation)
    return result.to_s
  end

  def self.present_quantity(item)
    return ProductUnit::EACH == item.product.unit ? '%x' % item.quantity.to_i : '%.3f' % item.quantity
  end

  def self.whitespace(whitespace_size)
    whitespace = ''
    whitespace_size.times do
      whitespace.concat(' ')
    end
    return whitespace
  end

end
