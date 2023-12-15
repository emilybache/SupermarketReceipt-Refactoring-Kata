defmodule Approvals do
  @moduledoc """
  A lightweight approval test helper, that uses the same files for expected
  values as the java version of the kata.

  To write an approval test, simply import this module, use `approve` instead
  of `test`, and `verify` a value instead of adding an assertion. The file
  paths are derived from the test module and name, so for example the file for
  this test …

  ```elixir
  defmodule Foo.BarTest do
    use ExUnit.Case, async: true
    import Approvals

    approve "baz generates some output" do
      verify Bar.baz()
    end
  end
  ```

  … would be `test/support/approvals/foo/bar_test/test_baz_generates_some_output`.

  This application has deliberately avoided using any external dependencies for
  simplicity. For approval testing in real projects, consider using
  [assert_value](https://github.com/assert-value/assert_value_elixir) or
  [mneme](https://github.com/zachallaun/mneme).
  """

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
