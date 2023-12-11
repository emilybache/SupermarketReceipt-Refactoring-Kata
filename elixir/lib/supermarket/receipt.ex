defmodule Supermarket.Receipt do
  defstruct []

  def add_product(receipt, _product, _quantity, _price, _total_price) do
    receipt
  end

  def add_discount(receipt, _discount) do
    receipt
  end
end
