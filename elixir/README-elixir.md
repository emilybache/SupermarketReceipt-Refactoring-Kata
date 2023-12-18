# README (Elixir)

This is the branch with approval tests. To run them:

    mix test

There are no external dependencies other than Elixir itself.

The approvals files (under `test/support/approvals`) are the same as the ones
from the java version of the kata, with the following exceptions:

  * Newlines have been added at the end of the last line of each file
  * Some values have been adjusted to match [Elixir’s handling of floating
    point rounding](https://hexdocs.pm/elixir/Float.html#round/2-known-issues),
    which differs from Java’s (eg 0.995 rounds to 0.99 rather than 1.00). This
    is an object lesson in not using floats for currency!
