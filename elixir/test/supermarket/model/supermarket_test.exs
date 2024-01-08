defmodule Supermarket.Model.SupermarketTest do
  use ExUnit.Case, async: true
  import Approvals

  alias Supermarket.Model.SupermarketCatalog
  alias Supermarket.Model.Product
  alias Supermarket.Model.ShoppingCart
  alias Supermarket.Model.Teller

  @toothbrush Product.new("toothbrush", :each)
  @rice Product.new("rice", :each)
  @apples Product.new("apples", :kilo)
  @cherry_tomatoes Product.new("cherry tomato box", :each)

  setup do
    catalog =
      FakeCatalog.new()
      |> SupermarketCatalog.add_product(@toothbrush, 0.99)
      |> SupermarketCatalog.add_product(@rice, 2.99)
      |> SupermarketCatalog.add_product(@apples, 1.99)
      |> SupermarketCatalog.add_product(@cherry_tomatoes, 0.69)

    teller = Teller.new(catalog)
    the_cart = ShoppingCart.new()
    %{catalog: catalog, teller: teller, the_cart: the_cart}
  end

  approve "an empty shopping cart should cost nothing", %{teller: teller, the_cart: the_cart} do
    receipt = Teller.checks_out_articles_from(teller, the_cart)
    verify ReceiptPrinter.print_receipt(receipt, 40)
  end

  approve "one normal item", %{teller: teller, the_cart: the_cart} do
    the_cart = ShoppingCart.add_item(the_cart, @toothbrush)
    receipt = Teller.checks_out_articles_from(teller, the_cart)
    verify ReceiptPrinter.print_receipt(receipt, 40)
  end

  approve "two normal items", %{teller: teller, the_cart: the_cart} do
    the_cart =
      the_cart
      |> ShoppingCart.add_item(@toothbrush)
      |> ShoppingCart.add_item(@rice)

    receipt = Teller.checks_out_articles_from(teller, the_cart)
    verify ReceiptPrinter.print_receipt(receipt, 40)
  end

  approve "buy two get one free", %{catalog: catalog, teller: teller, the_cart: the_cart} do
    the_cart =
      the_cart
      |> ShoppingCart.add_item(@toothbrush)
      |> ShoppingCart.add_item(@toothbrush)
      |> ShoppingCart.add_item(@toothbrush)

    teller =
      Teller.add_special_offer(
        teller,
        :three_for_two,
        @toothbrush,
        SupermarketCatalog.get_unit_price(catalog, @toothbrush)
      )

    receipt = Teller.checks_out_articles_from(teller, the_cart)
    verify ReceiptPrinter.print_receipt(receipt, 40)
  end

  approve "buy two get one free but insufficient in basket", %{
    catalog: catalog,
    teller: teller,
    the_cart: the_cart
  } do
    the_cart = ShoppingCart.add_item(the_cart, @toothbrush)

    teller =
      Teller.add_special_offer(
        teller,
        :three_for_two,
        @toothbrush,
        SupermarketCatalog.get_unit_price(catalog, @toothbrush)
      )

    receipt = Teller.checks_out_articles_from(teller, the_cart)
    verify ReceiptPrinter.print_receipt(receipt, 40)
  end

  approve "buy five get one free", %{catalog: catalog, teller: teller, the_cart: the_cart} do
    the_cart =
      the_cart
      |> ShoppingCart.add_item(@toothbrush)
      |> ShoppingCart.add_item(@toothbrush)
      |> ShoppingCart.add_item(@toothbrush)
      |> ShoppingCart.add_item(@toothbrush)
      |> ShoppingCart.add_item(@toothbrush)

    teller =
      Teller.add_special_offer(
        teller,
        :three_for_two,
        @toothbrush,
        SupermarketCatalog.get_unit_price(catalog, @toothbrush)
      )

    receipt = Teller.checks_out_articles_from(teller, the_cart)
    verify ReceiptPrinter.print_receipt(receipt, 40)
  end

  approve "loose weight product", %{teller: teller, the_cart: the_cart} do
    the_cart = ShoppingCart.add_item_quantity(the_cart, @apples, 0.5)
    receipt = Teller.checks_out_articles_from(teller, the_cart)
    verify ReceiptPrinter.print_receipt(receipt, 40)
  end

  approve "percent discount", %{teller: teller, the_cart: the_cart} do
    the_cart = ShoppingCart.add_item(the_cart, @rice)
    teller = Teller.add_special_offer(teller, :ten_percent_discount, @rice, 10.0)
    receipt = Teller.checks_out_articles_from(teller, the_cart)
    verify ReceiptPrinter.print_receipt(receipt, 40)
  end

  approve "x for y discount", %{teller: teller, the_cart: the_cart} do
    the_cart =
      the_cart
      |> ShoppingCart.add_item(@cherry_tomatoes)
      |> ShoppingCart.add_item(@cherry_tomatoes)

    teller = Teller.add_special_offer(teller, :two_for_amount, @cherry_tomatoes, 0.99)
    receipt = Teller.checks_out_articles_from(teller, the_cart)
    verify ReceiptPrinter.print_receipt(receipt, 40)
  end

  approve "x for y discount with insufficient in basket", %{teller: teller, the_cart: the_cart} do
    the_cart = ShoppingCart.add_item(the_cart, @cherry_tomatoes)
    teller = Teller.add_special_offer(teller, :two_for_amount, @cherry_tomatoes, 0.99)
    receipt = Teller.checks_out_articles_from(teller, the_cart)
    verify ReceiptPrinter.print_receipt(receipt, 40)
  end

  approve "Five for y discount", %{teller: teller, the_cart: the_cart} do
    the_cart = ShoppingCart.add_item_quantity(the_cart, @apples, 5)
    teller = Teller.add_special_offer(teller, :five_for_amount, @apples, 6.99)
    receipt = Teller.checks_out_articles_from(teller, the_cart)
    verify ReceiptPrinter.print_receipt(receipt, 40)
  end

  approve "Five for y discount with six", %{teller: teller, the_cart: the_cart} do
    the_cart = ShoppingCart.add_item_quantity(the_cart, @apples, 6)
    teller = Teller.add_special_offer(teller, :five_for_amount, @apples, 5.99)
    receipt = Teller.checks_out_articles_from(teller, the_cart)
    verify ReceiptPrinter.print_receipt(receipt, 40)
  end

  approve "Five for y discount with sixteen", %{teller: teller, the_cart: the_cart} do
    the_cart = ShoppingCart.add_item_quantity(the_cart, @apples, 16)
    teller = Teller.add_special_offer(teller, :five_for_amount, @apples, 7.99)
    receipt = Teller.checks_out_articles_from(teller, the_cart)
    verify ReceiptPrinter.print_receipt(receipt, 40)
  end

  approve "Five for y discount with four", %{teller: teller, the_cart: the_cart} do
    the_cart = ShoppingCart.add_item_quantity(the_cart, @apples, 4)
    teller = Teller.add_special_offer(teller, :five_for_amount, @apples, 8.99)
    receipt = Teller.checks_out_articles_from(teller, the_cart)
    verify ReceiptPrinter.print_receipt(receipt, 40)
  end
end
