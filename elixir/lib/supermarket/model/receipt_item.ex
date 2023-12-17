defmodule Supermarket.Model.ReceiptItem do
  defstruct [:product, :quantity, :price, :total_price]

  def new(product, quantity, price, total_price) do
    %__MODULE__{product: product, quantity: quantity, price: price, total_price: total_price}
  end
end
