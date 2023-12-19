# README (Elixir)

The code and tests are as close to the Java version as possible, with the
obvious changes related to immutability, dynamic types etc.

There are no external dependencies other than Elixir itself.

The main branch has a single ExUnit test, and the “with tests” branch contains
a more comprehensive set of approval tests, which use a tiny helper in
`test/support/approvals.ex` to avoid having to import an external approval
testing library.

The reference files for the approval tests (under `test/support/approvals`)
are the same as the ones from the java version of the kata, with the following
minor exceptions:

  * Newlines have been added at the end of the last line of each file
  * Some values have been adjusted to match [Elixir’s handling of floating
    point rounding](https://hexdocs.pm/elixir/Float.html#round/2-known-issues),
    which differs from Java’s (eg 0.995 rounds to 0.99 rather than 1.00). This
    is an object lesson in not using floats for currency!

For either branch, all tests can be run with the usual command:

    mix test
