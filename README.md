# The Supermarket Receipt Refactoring Kata 
### Jean-Marin RIBARIC & Joao DUARTE 

## Code smells:

- Product Units defined in ProductUnit.java as an enum.
- Special Offer Types defined in SpecialOfferType.java as an enum.
- Prices are in double type.

## Changes that should be made:

- There should be Units Each and Kilo class that inherit from a Product super class, with relevant function overrides.
- There should be a class for each special offer type that inherit from an Offer super class, with relevant function overrides.
- Prices should be represented in int type.
