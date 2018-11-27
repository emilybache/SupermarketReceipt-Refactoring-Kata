# The Supermarket Checkout exercise

This is a variation of a popular kata described in http://codekata.com/kata/kata01-supermarket-pricing/. The aim of the exercise is to build an automated teller that can check out articles from a shopping cart. 

The supermarket has a catalog with different types of products (rice, apples, milk, toothbrushes,...). Each product has a price, and the total price of the shopping cart is the total of all the prices.

But the supermarket also runs special deals, e.g.
 - Buy two toothbrushes, get one free
 - 10% discount on rice
 - 20% discount on apples if you buy more than 10
 - Bags of 1 kg of oranges $4 instead of $5.

These are just examples: the actual special deals changes each week, so needs to be easily configurable.

## Goal

The goal of the exercise is to implement a teller that can handle the following scenarios (and more - use your imagination!)

 - The teller should be able to handle a shopping cart with no special deals
 - The client should get a receipt with the list of purchases and the total price.
 - The teller should be able to handle the following scenarios
    - Buy 2 get one free
    - Buy 4 get one free
    - 10% discount on a certain product (e.g. 10% discount on 1kg packets of rice)
    - 20% discount on a certain product if you buy more than 10 (e.g 20% discount on apples for 11 or more apples)
    - Fixed discounts (e.g. bag of 1kg of oranges costs $4 instead of $5).
 
 - The teller should be able to handle combinations of the above scenarios, when there is more than one special deal in the shopping cart items.

There is a simple failing test in `WhenCheckingOutArticlesAtTheSupermarket` to get you started.

