# Supermarket Receipt in [Python](https://www.python.org/)

## Setup

* Have Python installed
* Clone the repository
* On the command line, enter the `SupermarketReceipt-Refactoring-Kata/python` directory
* On the command line, install requirements, e.g. on the`python -m pip install -r requirements.txt`

## Running Tests

On the command line, enter the `SupermarketReceipt-Refactoring-Kata/python` directory and run

```
pytest
```

## Optional: Running [TextTest](https://www.texttest.org/) Tests

Install TextTest according to the [instructions](https://www.texttest.org/index.html#getting-started-with-texttest) (platform specific).

On the command line, enter the `SupermarketReceipt-Refactoring-Kata/python` directory and run

```
texttest -a sr -d .
```

## Refactor
we can always solve and enhance.. but here's the selected enhancments:
* model_objects.py lacked Separation of Concerns and Type Annotations
* enforced Typing including List, Dict, and defaultdict
* added some validations, examples: 
    - validate no overflow in the columns size at def format_line_with_whitespace().
    - validate non-negative prams passed to add_product.
* ShoppingCart sperate some specific logic Ex: offers into private methods:
    for easier maintainability, avoiding duplication and applying encapsulation.
* ShoppingCart.handle_offers: code enhancment: 
    - remove neasted if statments for easier maintainability and debugging.
    - enhance variables names Ex: p into product
* Adapting Strategy pattern for the offers calculations.
* Used int with cents instead of float on amount and prices to avoid rounding issues.