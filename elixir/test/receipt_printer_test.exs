defmodule ReceiptPrinterTest do
  use ExUnit.Case, async: true

  alias Supermarket.Discount
  alias Supermarket.Product
  alias Supermarket.Receipt

  @toothbrush %Product{name: "toothbrush", unit: :each}
  @apples %Product{name: "apples", unit: :kilo}

  setup do
    %{receipt: %Receipt{}}
  end

  test "one line item", %{receipt: receipt} do
    receipt = Receipt.add_product(receipt, @toothbrush, 1, 0.99, 0.99)
    Approvals.verify(ReceiptPrinter.print_receipt(receipt, 40))
  end

  test "quantity two", %{receipt: receipt} do
    receipt = Receipt.add_product(receipt, @toothbrush, 2, 0.99, 0.99 * 2)
    Approvals.verify(ReceiptPrinter.print_receipt(receipt, 40))
  end

  test "loose weight", %{receipt: receipt} do
    receipt = Receipt.add_product(receipt, @apples, 2.3, 1.99, 1.99 * 2.3)
    Approvals.verify(ReceiptPrinter.print_receipt(receipt, 40))
  end

  test "total", %{receipt: receipt} do
    receipt =
      receipt
      |> Receipt.add_product(@toothbrush, 1, 0.99, 2 * 0.99)
      |> Receipt.add_product(@apples, 0.75, 1.99, 1.99 * 0.75)

    Approvals.verify(ReceiptPrinter.print_receipt(receipt, 40))
  end

  test "discounts", %{receipt: receipt} do
    receipt =
      Receipt.add_discount(receipt, %Discount{
        product: @apples,
        description: "3 for 2",
        discount_amount: -0.99
      })

    Approvals.verify(ReceiptPrinter.print_receipt(receipt, 40))
  end

  test "print whole receipt", %{receipt: receipt} do
    receipt =
      receipt
      |> Receipt.add_product(@toothbrush, 1, 0.99, 0.99)
      |> Receipt.add_product(@toothbrush, 2, 0.99, 0.99 * 2)
      |> Receipt.add_product(@apples, 0.75, 1.99, 1.99 * 0.75)
      |> Receipt.add_discount(%Discount{
        product: @toothbrush,
        description: "3 for 2",
        discount_amount: -0.99
      })

    Approvals.verify(ReceiptPrinter.print_receipt(receipt, 40))
  end
end
