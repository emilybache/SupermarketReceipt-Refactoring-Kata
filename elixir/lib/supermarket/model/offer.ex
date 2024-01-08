defmodule Supermarket.Model.Offer do
  defstruct [:offer_type, :product, :argument]

  def new(offer_type, product, argument) do
    %__MODULE__{offer_type: offer_type, argument: argument, product: product}
  end
end
