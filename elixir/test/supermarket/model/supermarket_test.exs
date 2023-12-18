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

  setup do
    catalog =
      FakeCatalog.new()
      |> SupermarketCatalog.add_product(@toothbrush, 0.99)
      |> SupermarketCatalog.add_product(@rice, 2.99)
      |> SupermarketCatalog.add_product(@apples, 1.99)

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

  @java """
  package dojo.supermarket.model;

  import dojo.supermarket.ReceiptPrinter;
  import org.approvaltests.Approvals;
  import org.junit.jupiter.api.BeforeEach;
  import org.junit.jupiter.api.Test;

  public class SupermarketTest {
      private SupermarketCatalog catalog;
      private Teller teller;
      private ShoppingCart theCart;
      private Product toothbrush;
      private Product rice;
      private Product apples;
      private Product cherryTomatoes;

      @BeforeEach
      public void setUp() {
          catalog = new FakeCatalog();
          teller = new Teller(catalog);
          theCart = new ShoppingCart();

          toothbrush = new Product("toothbrush", ProductUnit.EACH);
          catalog.addProduct(toothbrush, 0.99);
          rice = new Product("rice", ProductUnit.EACH);
          catalog.addProduct(rice, 2.99);
          apples = new Product("apples", ProductUnit.KILO);
          catalog.addProduct(apples, 1.99);
          cherryTomatoes = new Product("cherry tomato box", ProductUnit.EACH);
          catalog.addProduct(cherryTomatoes, 0.69);

      }

      @Test
      public void xForY_discount() {
          theCart.addItem(cherryTomatoes);
          theCart.addItem(cherryTomatoes);
          teller.addSpecialOffer(SpecialOfferType.TWO_FOR_AMOUNT, cherryTomatoes,.99);
          Receipt receipt = teller.checksOutArticlesFrom(theCart);
          Approvals.verify(new ReceiptPrinter(40).printReceipt(receipt));
      }

      @Test
      public void xForY_discount_with_insufficient_in_basket() {
          theCart.addItem(cherryTomatoes);
          teller.addSpecialOffer(SpecialOfferType.TWO_FOR_AMOUNT, cherryTomatoes,.99);
          Receipt receipt = teller.checksOutArticlesFrom(theCart);
          Approvals.verify(new ReceiptPrinter(40).printReceipt(receipt));
      }

      @Test
      public void FiveForY_discount() {
          theCart.addItemQuantity(apples, 5);
          teller.addSpecialOffer(SpecialOfferType.FIVE_FOR_AMOUNT, apples,6.99);
          Receipt receipt = teller.checksOutArticlesFrom(theCart);
          Approvals.verify(new ReceiptPrinter(40).printReceipt(receipt));
      }

      @Test
      public void FiveForY_discount_withSix() {
          theCart.addItemQuantity(apples, 6);
          teller.addSpecialOffer(SpecialOfferType.FIVE_FOR_AMOUNT, apples,5.99);
          Receipt receipt = teller.checksOutArticlesFrom(theCart);
          Approvals.verify(new ReceiptPrinter(40).printReceipt(receipt));
      }

      @Test
      public void FiveForY_discount_withSixteen() {
          theCart.addItemQuantity(apples, 16);
          teller.addSpecialOffer(SpecialOfferType.FIVE_FOR_AMOUNT, apples,7.99);
          Receipt receipt = teller.checksOutArticlesFrom(theCart);
          Approvals.verify(new ReceiptPrinter(40).printReceipt(receipt));
      }

      @Test
      public void FiveForY_discount_withFour() {
          theCart.addItemQuantity(apples, 4);
          teller.addSpecialOffer(SpecialOfferType.FIVE_FOR_AMOUNT, apples,8.99);
          Receipt receipt = teller.checksOutArticlesFrom(theCart);
          Approvals.verify(new ReceiptPrinter(40).printReceipt(receipt));
      }
  }
  """
end
