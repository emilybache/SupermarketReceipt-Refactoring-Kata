require_relative './test_helper'

class ReceiptPrinterTest < Minitest::Test
  include Approvals

  cover "Kata*"

  def setup
    @toothbrush = Kata::Product.new("toothbrush", Kata::ProductUnit::EACH)
    @apples = Kata::Product.new("apples", Kata::ProductUnit::KILO)
    @receipt = Kata::Receipt.new
  end

  def test_one_line_item
    @receipt.add_product(@toothbrush, 1, 0.99, 0.99)
    verify Kata::ReceiptPrinter.new(40).print_receipt(@receipt)
  end

  def test_quantity_two
    @receipt.add_product(@toothbrush, 2, 0.99, 0.99 * 2)
    verify Kata::ReceiptPrinter.new(40).print_receipt(@receipt)
  end

  def test_loose_weight
    @receipt.add_product(@apples, 2.3, 1.99, 1.99 * 2.3)
    verify Kata::ReceiptPrinter.new(40).print_receipt(@receipt)
  end

  def test_total
    @receipt.add_product(@toothbrush, 1, 0.99, 2 * 0.99)
    @receipt.add_product(@apples, 0.75, 1.99, 1.99*0.75)
    verify Kata::ReceiptPrinter.new(40).print_receipt(@receipt)
  end

  def test_discounts
    @receipt.add_discount(Kata::Discount.new(@apples, "3 for 2", 0.99))
    verify Kata::ReceiptPrinter.new(40).print_receipt(@receipt)
  end

  def test_print_whole_receipt
    @receipt.add_product(@toothbrush, 1, 0.99, 0.99)
    @receipt.add_product(@toothbrush, 2, 0.99, 2*0.99)
    @receipt.add_product(@apples, 0.75, 1.99, 1.99*0.75)
    @receipt.add_discount(Kata::Discount.new(@toothbrush, "3 for 2", 0.99))
    verify Kata::ReceiptPrinter.new(40).print_receipt(@receipt)
  end

end
