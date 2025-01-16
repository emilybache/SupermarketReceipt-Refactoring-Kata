# The Supermarket Receipt Refactoring Kata 
### Jean-Marin RIBARIC & Joao DUARTE 

## Code smells:

- Product Units defined in ProductUnit.java as an enum.
- Special Offer Types defined in SpecialOfferType.java as an enum.
- Prices are in double type.
- handleOffers method in ShoppingCart.java is too long and has too many if statements.
- Defining certain attributes multiple times, ex: product in Offer.java & ProductQuantity.java

## Changes that should be made:

- There should be Units Each and Kilo class that inherit from a Product super class, with relevant function overrides.
- There should be a class for each special offer type that inherit from an Offer super class, with relevant function overrides.
- Prices should be represented in int type.
- handleOffers method: creating a "calculateOffer" method for each offer type, calling that method once instead of using if statements.
- Defining certain attributes multiple times: Possibly creating a singleton with the product data
