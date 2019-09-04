class ShoppingCart

  def initialize
    @items = []
    @product_quantities = {}
  end

  def items
    Array.new @items
  end

  def add_item(product)
    add_item_quantity(product, 1.0)
    nil
  end

  def product_quantities
    @product_quantities
  end

  def add_item_quantity(product, quantity)
    @items << ProductQuantity.new(product, quantity)
    if @product_quantities.key?(product)
      product_quantities[product] = product_quantities[product] + quantity
    else
      product_quantities[product] = quantity
    end
  end

  def handle_offers(receipt, offers, catalog)
    for p in @product_quantities.keys do
      quantity = @product_quantities[p]
      if offers.key?(p)
        offer = offers[p]
        unit_price = catalog.unit_price(p)
        quantity_as_int = quantity.to_i
        discount = nil
        x = 1
        if offer.offer_type == SpecialOfferType::THREE_FOR_TWO
          x = 3

        elsif offer.offer_type == SpecialOfferType::TWO_FOR_AMOUNT
          x = 2
          if quantity_as_int >= 2
            total = offer.argument * (quantity_as_int / x) + quantity_as_int % 2 * unit_price
            discount_n = unit_price * quantity - total
            discount = Discount.new(p, "2 for " + offer.argument.to_s, discount_n)
          end

        end
        if offer.offer_type == SpecialOfferType:: FIVE_FOR_AMOUNT
          x = 5
        end
        number_of_x = quantity_as_int / x
        if offer.offer_type == SpecialOfferType::THREE_FOR_TWO && quantity_as_int > 2
          discount_amount = quantity * unit_price - ((number_of_x * 2 * unit_price) + quantity_as_int % 3 * unit_price)
          discount = Discount.new(p, "3 for 2", discount_amount)
        end
        if offer.offer_type == SpecialOfferType::TEN_PERCENT_DISCOUNT
          discount = Discount.new(p, offer.argument.to_s + "% off", quantity * unit_price * offer.argument / 100.0)
        end
        if offer.offer_type == SpecialOfferType::FIVE_FOR_AMOUNT && quantity_as_int >= 5
          discount_total = unit_price * quantity - (offer.argument * number_of_x + quantity_as_int % 5 * unit_price)
          discount = Discount.new(p, x.to_s + " for " + offer.argument.to_s, discount_total)
        end

        receipt.add_discount(discount) if discount
      end
    end
  end

end
