defmodule Supermarket.Model.Receipt do
  defstruct items: [], discounts: []

  alias Supermarket.Model.ReceiptItem

  def add_product(receipt, product, quantity, price, total_price) do
    item = %ReceiptItem{
      product: product,
      quantity: quantity,
      price: price,
      total_price: total_price
    }

    Map.update!(receipt, :items, &[item | &1])
  end

  def add_discount(receipt, discount) do
    Map.update!(receipt, :discounts, &[discount | &1])
  end

  def total_price(receipt) do
    item_total = Enum.reduce(receipt.items, 0.0, fn item, total -> item.total_price + total end)

    discount_total =
      Enum.reduce(receipt.discounts, 0.0, fn discount, total ->
        discount.discount_amount + total
      end)

    item_total + discount_total
  end
end
