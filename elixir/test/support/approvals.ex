defmodule Approvals do
  defmacro approve(name, context \\ %{}, expr) do
    quote do
      test unquote(name), unquote(context) = %{module: approvals_module, test: approvals_test} do
        var!(approvals_file) =
          approvals_module
          |> Macro.underscore()
          |> Path.join(approvals_test |> to_string |> String.replace(~r/\W/, "_"))
          |> Path.expand("test/support/approvals")

        unquote(expr)
      end
    end
  end

  defmacro verify(expr) do
    quote do
      assert unquote(expr) == File.read!(var!(approvals_file))
    end
  end
end
