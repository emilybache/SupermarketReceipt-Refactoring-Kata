defmodule ReceiptPrinter do
  alias Supermarket.Model.Receipt

  def print_receipt(receipt, columns \\ 40) do
    IO.chardata_to_string([
      receipt.items |> Enum.reverse() |> Enum.map(&[present_receipt_item(&1, columns), "\n"]),
      receipt.discounts |> Enum.reverse() |> Enum.map(&[present_discount(&1, columns), "\n"]),
      "\n",
      present_total(receipt, columns)
    ])
  end

  defp present_receipt_item(item, columns) do
    total_price_presentation = present_price(item.total_price)
    name = item.product.name

    format_line_with_whitespace(name, total_price_presentation, columns)
    |> append_price_breakdown_unless_single_unit(item)
  end

  defp append_price_breakdown_unless_single_unit(line, item) do
    if item.quantity == 1 do
      line
    else
      "#{line}\n  #{present_price(item.price)} * #{present_quantity(item)}"
    end
  end

  defp present_discount(discount, columns) do
    name = "#{discount.description}(#{discount.product.name})"
    value = present_price(discount.discount_amount)
    format_line_with_whitespace(name, value, columns)
  end

  defp present_total(receipt, columns) do
    name = "Total: "
    value = receipt |> Receipt.total_price() |> present_price()
    format_line_with_whitespace(name, value, columns)
  end

  defp format_line_with_whitespace(name, value, columns) do
    String.pad_trailing(name, columns - String.length(value)) <> value
  end

  defp present_price(price), do: :io_lib.format("~.2f", [price]) |> to_string()

  def present_quantity(item) do
    if item.product.unit == :each do
      item.quantity
    else
      :io_lib.format("~.3f", [item.quantity]) |> to_string()
    end
  end
end
