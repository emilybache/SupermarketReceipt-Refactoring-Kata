require_relative './test_helper'

class SupermarketTest < Minitest::Test
  include Approvals

  def setup
    @catalog = FakeCatalog.new
    @teller = Teller.new(@catalog)
    @the_cart = ShoppingCart.new

    @toothbrush = Product.new("toothbrush", ProductUnit::EACH)
    @catalog.add_product(@toothbrush, 0.99)
    @rice = Product.new("rice", ProductUnit::EACH)
    @catalog.add_product(@rice, 2.99)
    @apples = Product.new("apples", ProductUnit::KILO)
    @catalog.add_product(@apples, 1.99)
    @cherry_tomatoes = Product.new("cherry tomato box", ProductUnit::EACH)
    @catalog.add_product(@cherry_tomatoes, 0.69)
  end

  def test_an_empty_shopping_cart_should_cost_nothing
    receipt = @teller.checks_out_articles_from(@the_cart)
    verify ReceiptPrinter.new(40).print_receipt(receipt)
  end

  def test_one_normal_item
    @the_cart.add_item(@toothbrush)
    receipt = @teller.checks_out_articles_from(@the_cart)
    verify ReceiptPrinter.new(40).print_receipt(receipt)
  end

  def test_two_normal_items
    @the_cart.add_item(@toothbrush)
    @the_cart.add_item(@rice)
    receipt = @teller.checks_out_articles_from(@the_cart)
    verify ReceiptPrinter.new(40).print_receipt(receipt)
  end

  def test_buy_two_get_one_free
    @the_cart.add_item(@toothbrush)
    @the_cart.add_item(@toothbrush)
    @the_cart.add_item(@toothbrush)
    @teller.add_special_offer(SpecialOfferType::THREE_FOR_TWO, @toothbrush, @catalog.unit_price(@toothbrush))
    receipt = @teller.checks_out_articles_from(@the_cart)
    verify ReceiptPrinter.new(40).print_receipt(receipt)
  end

  def test_buy_five_get_one_free
    @the_cart.add_item(@toothbrush)
    @the_cart.add_item(@toothbrush)
    @the_cart.add_item(@toothbrush)
    @the_cart.add_item(@toothbrush)
    @the_cart.add_item(@toothbrush)
    @teller.add_special_offer(SpecialOfferType::THREE_FOR_TWO, @toothbrush, @catalog.unit_price(@toothbrush))
    receipt = @teller.checks_out_articles_from(@the_cart)
    verify ReceiptPrinter.new(40).print_receipt(receipt)
  end

  def test_loose_weight_product
    @the_cart.add_item_quantity(@apples, 0.5)
    receipt = @teller.checks_out_articles_from(@the_cart)
    verify ReceiptPrinter.new(40).print_receipt(receipt)
  end

  def test_percent_discount
    @the_cart.add_item(@rice)
    @teller.add_special_offer(SpecialOfferType::TEN_PERCENT_DISCOUNT, @rice, 10.0)
    receipt = @teller.checks_out_articles_from(@the_cart)
    verify ReceiptPrinter.new(40).print_receipt(receipt)
  end

  def test_x_for_y_discount
    @the_cart.add_item(@cherry_tomatoes)
    @the_cart.add_item(@cherry_tomatoes)
    @teller.add_special_offer(SpecialOfferType::TWO_FOR_AMOUNT, @cherry_tomatoes, 0.99)
    receipt = @teller.checks_out_articles_from(@the_cart)
    verify ReceiptPrinter.new(40).print_receipt(receipt)
  end

  def test_five_for_y_discount
    @the_cart.add_item_quantity(@apples, 5)
    @teller.add_special_offer(SpecialOfferType::FIVE_FOR_AMOUNT, @apples, 6.99)
    receipt = @teller.checks_out_articles_from(@the_cart)
    verify ReceiptPrinter.new(40).print_receipt(receipt)
  end

  def test_five_for_y_discount_with_six
    @the_cart.add_item_quantity(@apples, 6)
    @teller.add_special_offer(SpecialOfferType::FIVE_FOR_AMOUNT, @apples, 6.99)
    receipt = @teller.checks_out_articles_from(@the_cart)
    verify ReceiptPrinter.new(40).print_receipt(receipt)
  end

  def test_five_for_y_discount_with_sixteen
    @the_cart.add_item_quantity(@apples, 16)
    @teller.add_special_offer(SpecialOfferType::FIVE_FOR_AMOUNT, @apples, 6.99)
    receipt = @teller.checks_out_articles_from(@the_cart)
    verify ReceiptPrinter.new(40).print_receipt(receipt)
  end

  def test_five_for_y_discount_with_four
    @the_cart.add_item_quantity(@apples, 4)
    @teller.add_special_offer(SpecialOfferType::FIVE_FOR_AMOUNT, @apples, 6.99)
    receipt = @teller.checks_out_articles_from(@the_cart)
    verify ReceiptPrinter.new(40).print_receipt(receipt)
  end

end
