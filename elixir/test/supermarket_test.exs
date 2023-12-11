defmodule SupermarketTest do
  use ExUnit.Case
  doctest Supermarket

  test "greets the world" do
    assert Supermarket.hello() == :world
  end
end
