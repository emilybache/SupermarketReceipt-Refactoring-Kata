defmodule Supermarket.Model.SupermarketTest do
  use ExUnit.Case, async: true

  alias Supermarket.Model.Product
  alias Supermarket.Model.Receipt
  alias Supermarket.Model.ShoppingCart
  alias Supermarket.Model.SupermarketCatalog
  alias Supermarket.Model.Teller

  # Todo: test all kinds of discounts are applied properly

  test "ten percent discount" do
    toothbrush = Product.new("toothbrush", :each)
    apples = Product.new("apples", :kilo)

    catalog =
      FakeCatalog.new()
      |> SupermarketCatalog.add_product(toothbrush, 0.99)
      |> SupermarketCatalog.add_product(apples, 1.99)

    teller =
      catalog
      |> Teller.new()
      |> Teller.add_special_offer(:ten_percent_discount, toothbrush, 10.0)

    the_cart = ShoppingCart.new() |> ShoppingCart.add_item_quantity(apples, 2.5)

    # ACT
    receipt = Teller.checks_out_articles_from(teller, the_cart)

    # ASSERT
    assert_in_delta Receipt.total_price(receipt), 4.975, 0.01
    assert receipt.discounts == []
    assert length(receipt.items) == 1
    receipt_item = List.first(receipt.items)
    assert receipt_item.product == apples
    assert receipt_item.price == 1.99
    assert receipt_item.total_price == 2.5 * 1.99
    assert receipt_item.quantity == 2.5
  end
end
