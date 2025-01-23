# Pull request: Refactor code for clarity, single responsibility, and better test coverage

This md references the suggestions that will be mentioned in the pull request that identifies some code smells in the project and suggests the following remedies to them.

1. **Improve Variable Naming:**
    - Rename the variable `argument` in the `Offer` class to `discountAmount` to have more clarity and reflect its purpose better.
    - Update all the references to this variable across the project.

2. **Refactor Long Classes with Too Much Responsibility:**
    - The `ShoppingCart` class handles too many things; it's related to products, quantities, and discounts, and it even has a `handleOffers` method.
    - These responsibilities could perhaps be delegated and split into more specific classes. For example, create a new `CartItem` class to handle product and quantity data, and extract the discount-related logic into a new `CalculateDiscounts` class.

3. **Expand Test Coverage:**
    - The `SupermarketTest` class is in charge of checking the functionality of the discounts that can be applied. However, although it has a test method `tenPercentDiscount()` that checks that the `TEN_PERCENT_DISCOUNT` works, it is missing the other three discount types.
    - Add the test methods missing to cover all discount types: `THREE_FOR_TWO`, `TWO_FOR_AMOUNT`, and `FIVE_FOR_AMOUNT`.

## Benefits of These Recommendations

- **Improved Readability:** Variable names would be more descriptive, making the code easier to understand.
- **Better Clarity and Organization:** Classes adhering to a single responsibility approach reduce complexity and improve maintainability.
- **More Test Coverage:** Additional tests ensure reliability for all discount scenarios.
