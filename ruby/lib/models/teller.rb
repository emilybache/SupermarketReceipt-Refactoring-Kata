class Teller

  def initialize(catalog)
    @catalog = catalog
    @offers = {}
  end

  def add_special_offer(offer_type, product, argument)
    @offers[product] = Offer.new(offer_type, product, argument)
  end

  def checks_out_articles_from(the_cart)
    receipt = Receipt.new
    product_quantities = the_cart.items
    for pq in product_quantities do
      p = pq.product
      quantity = pq.quantity
      unit_price = @catalog.unit_price(p)
      price = quantity * unit_price
      receipt.add_product(p, quantity, unit_price, price)
    end
    the_cart.handle_offers(receipt, @offers, @catalog)

    receipt
  end

end
