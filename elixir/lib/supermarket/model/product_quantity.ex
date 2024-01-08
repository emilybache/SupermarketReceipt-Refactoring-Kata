defmodule Supermarket.Model.ProductQuantity do
  defstruct [:product, :quantity]

  def new(product, weight), do: %__MODULE__{product: product, quantity: weight}
end
