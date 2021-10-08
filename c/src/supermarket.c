#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "supermarket.h"

struct product_t* product_create(char* name, enum unit unit) {
    struct product_t* product = malloc(sizeof(*product));
    strncpy(product->name, name, sizeof(product->name) - 1);
    product->unit = unit;
    return product;
}

struct catalog_t *catalog_create(struct product_t *products, const double *prices, int product_count) {
    struct catalog_t* catalog = malloc(sizeof(*catalog));
    catalog->product_count = product_count;
    for (int i = 0; i < product_count; ++i) {
        catalog->products[i] = products[i];
        catalog->prices[i] = prices[i];
    }
    return catalog;
}

struct teller_t *teller_create(struct catalog_t *catalog, int product_count, struct special_offer_t *offer) {
    struct teller_t* teller = malloc(sizeof(*teller));
    teller->catalog = catalog;
    teller->offer = offer;
    return teller;
}

struct special_offer_t* special_offer_create(enum SpecialOfferType type, struct product_t *product, float argument) {
    struct special_offer_t* special_offer = malloc(sizeof(*special_offer));
    special_offer->type = type;
    special_offer->product = product;
    special_offer->argument = argument;

    return special_offer;
}

struct cart_t* cart_create(struct product_t products[], const double quantities[], int product_count) {
    struct cart_t* cart = malloc(sizeof(*cart));
    cart->product_count = product_count;
    for (int i = 0; i < product_count; ++i) {
        cart->products[i] = products[i];
        cart->quantities[i] = quantities[i];
    }
    return cart;
}

struct receipt_item_t *
receipt_item_create(struct product_t* product, double quantity, double price, double totalPrice) {
    struct receipt_item_t* item = malloc(sizeof(*item));
    item->product = product;
    item->quantity = quantity;
    item->price = price;
    item->totalPrice = totalPrice;
    return item;
}

struct discount_t* discount_create(char* description, double amount, struct product_t* product) {
    struct discount_t* discount = malloc(sizeof(discount));
    discount->product = product;
    strcpy(discount->description, description);
    discount->amount = amount;

    return discount;
}

void handle_offers(struct cart_t* cart, struct receipt_t* receipt, struct special_offer_t* offer, struct catalog_t* catalog) {
    for (int i = 0; i < cart->product_count; ++i) {
        struct product_t product = cart->products[i];
        double quantity = cart->quantities[i];
        if (strcmp(offer->product->name, product.name) == 0) {
            double unitPrice = unit_price(catalog, &product);
            int quantityAsInt = (int)quantity;
            struct discount_t* discount = NULL;
            int x = 1;

            if (offer->type == ThreeForTwo) {
                x = 3;
            } else if (offer->type == TwoForAmount) {
                x = 2;
                if (quantityAsInt >= 2) {
                    double total = offer->argument * (quantityAsInt / x) + quantityAsInt % 2 * unitPrice;
                    double discountN = unitPrice * quantity - total;
                    char description[MAX_NAME_LENGTH];
                    sprintf(description, "2 for %f", offer->argument);
                    discount = discount_create(description, -discountN, &product);
                }
            } if (offer->type == FiveForAmount) {
                x = 5;
            }
            int numberOfXs = quantityAsInt / x;
            if (offer->type == ThreeForTwo && quantityAsInt > 2) {
                double discountAmount = quantity * unitPrice - ((numberOfXs * 2 * unitPrice) + quantityAsInt % 3 * unitPrice);
                char description[MAX_NAME_LENGTH];
                sprintf(description, "3 for 2");
                discount = discount_create(description, -discountAmount, &product);
            }
            if (offer->type == TenPercentDiscount) {
                char description[MAX_NAME_LENGTH];
                sprintf(description, "%.0f%% off", offer->argument);
                discount = discount_create(description, -quantity * unitPrice * offer->argument / 100.0, &product);

            }
            if (offer->type == FiveForAmount && quantityAsInt >= 5) {
                double discountTotal = unitPrice * quantity - (offer->argument * numberOfXs + quantityAsInt % 5 * unitPrice);
                char description[MAX_NAME_LENGTH];
                sprintf(description, "%d for %f", x, offer->argument);
                discount = discount_create(description, -discountTotal, &product);
            }
            if (discount != NULL) {
                receipt->discounts[receipt->discountCount] = *discount;
                receipt->discountCount++;
            }
        }
    }
}

struct receipt_t *check_out_articles(struct teller_t* teller, struct cart_t* cart) {
    struct receipt_t* receipt = malloc(sizeof(*receipt));
    receipt->itemCount = cart->product_count;
    for (int i = 0; i < cart->product_count; ++i) {
        double uprice = unit_price(teller->catalog, &cart->products[i]);
        double price = uprice * cart->quantities[i];
        struct receipt_item_t* item = receipt_item_create(&cart->products[i], cart->quantities[i], uprice, price);
        receipt->items[i] = *item;
    }
    handle_offers(cart, receipt, teller->offer, teller->catalog);

    return receipt;
}

double total_price(struct receipt_t *receipt) {
    double total = 0;
    for (int i = 0; i < receipt->itemCount; ++i) {
        total += receipt->items[i].totalPrice;
    }
    for (int i = 0; i < receipt->discountCount; ++i) {
        total += receipt->discounts[i].amount;
    }
    return total;
}


double unit_price(struct catalog_t* catalog, struct product_t *product) {
    for (int i = 0; i < catalog->product_count; ++i) {
        if (strcmp(catalog->products[i].name, product->name) == 0)
            return catalog->prices[i];
    }
    return 0;
}
