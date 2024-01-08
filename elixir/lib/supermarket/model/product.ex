defmodule Supermarket.Model.Product do
  defstruct [:name, :unit]

  def new(name, unit), do: %__MODULE__{name: name, unit: unit}
end
