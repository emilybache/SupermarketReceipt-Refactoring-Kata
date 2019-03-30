require_relative './test_helper'

class SupermarketTest < Minitest::Test

  def test_ten_percent_discount
    catalog = FakeCatalog.new
    toothbrush = Product.new("toothbrush", ProductUnit::EACH)
    catalog.add_product(toothbrush, 0.99)

    apples = Product.new("apples", ProductUnit::KILO)
    catalog.add_product(apples, 1.99)

    cart = ShoppingCart.new
    cart.add_item_quantity(apples, 2.5)

    teller = Teller.new(catalog)
    teller.add_special_offer(SpecialOfferType::TEN_PERCENT_DISCOUNT, toothbrush, 10.0)

    receipt = teller.checks_out_articles_from(cart)

    assert_in_delta 4.975, receipt.total_price, 0.01
  end

end
