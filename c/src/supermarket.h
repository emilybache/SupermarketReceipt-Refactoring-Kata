#ifndef SAMPLE_H
#define SAMPLE_H

enum unit {
    Each,
    Kilo
};

enum SpecialOfferType {
    TenPercentDiscount,
    ThreeForTwo,
    TwoForAmount,
    FiveForAmount
};

#define MAX_NAME_LENGTH 100
#define MAX_PRODUCTS 100

struct product_t {
    char name[MAX_NAME_LENGTH];
    enum unit unit;
};

struct catalog_t {
    int product_count;
    struct product_t products[MAX_PRODUCTS];
    double prices[MAX_PRODUCTS];
};

struct special_offer_t {
    enum SpecialOfferType type;
    struct product_t* product;
    float argument;
};

struct teller_t {
    struct catalog_t* catalog;
    struct special_offer_t* offer;
};

struct cart_t {
    int product_count;
    struct product_t products[MAX_PRODUCTS];
    double quantities[MAX_PRODUCTS];
};

struct receipt_item_t {
    struct product_t* product;
    double quantity;
    double price;
    double totalPrice;
};

struct discount_t {
    struct product_t* product;
    char description[MAX_NAME_LENGTH];
    double amount;
};

struct receipt_t {
    int itemCount;
    struct receipt_item_t items[MAX_PRODUCTS];
    int discountCount;
    struct discount_t discounts[MAX_PRODUCTS];
};

struct product_t* product_create(char* name, enum unit unit);
struct special_offer_t* special_offer_create(enum SpecialOfferType type, struct product_t *product, float argument);
struct catalog_t* catalog_create(struct product_t products[], const double prices[], int product_count);
struct teller_t *teller_create(struct catalog_t *products, int product_count, struct special_offer_t *offer);
struct cart_t* cart_create(struct product_t products[], const double quantities[], int product_count);
struct receipt_item_t* receipt_item_create(struct product_t* product, double quantity, double price, double totalPrice);
struct discount_t* discount_create(char* description, double discount, struct product_t* product);

struct receipt_t* check_out_articles(struct teller_t* teller, struct cart_t* cart);
double total_price(struct receipt_t* receipt);
double unit_price(struct catalog_t* catalog, struct product_t *product);
void handle_offers(struct cart_t* cart, struct receipt_t* receipt, struct special_offer_t* offer, struct catalog_t* catalog);

#endif //SAMPLE_H
