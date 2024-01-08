defmodule Supermarket.Model.Teller do
  alias Supermarket.Model.Offer
  alias Supermarket.Model.Receipt
  alias Supermarket.Model.ShoppingCart
  alias Supermarket.Model.SupermarketCatalog

  defstruct [:catalog, :offers]

  def new(catalog) do
    %__MODULE__{catalog: catalog, offers: %{}}
  end

  def add_special_offer(teller, offer_type, product, argument) do
    Map.update!(teller, :offers, &Map.put(&1, product, Offer.new(offer_type, product, argument)))
  end

  def checks_out_articles_from(teller, the_cart) do
    receipt = Receipt.new()
    product_quantities = the_cart.items

    receipt =
      product_quantities
      |> Enum.reverse()
      |> Enum.reduce(receipt, fn pq, receipt ->
        p = pq.product
        quantity = pq.quantity
        unit_price = SupermarketCatalog.get_unit_price(teller.catalog, p)
        price = quantity * unit_price
        Receipt.add_product(receipt, p, quantity, unit_price, price)
      end)

    ShoppingCart.handle_offers(the_cart, receipt, teller.offers, teller.catalog)
  end
end
