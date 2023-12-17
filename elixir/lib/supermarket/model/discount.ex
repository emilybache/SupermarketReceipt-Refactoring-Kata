defmodule Supermarket.Model.Discount do
  defstruct [:product, :description, :discount_amount]

  def new(product, description, discount_amount) do
    %__MODULE__{product: product, description: description, discount_amount: discount_amount}
  end
end
