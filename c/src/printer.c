#include <stdio.h>
#include <string.h>
#include "printer.h"

void printReceiptItem(char *buffer, struct receipt_item_t *item);

void printDiscount(char *buffer, struct discount_t *pDiscount);

void printTotal(const char *buffer, struct receipt_t *receipt);

void print_receipt(char* buffer, struct receipt_t* receipt) {
    for (int i = 0; i < receipt->itemCount; ++i) {
        printReceiptItem(buffer, &receipt->items[i]);
    }
    for (int i = 0; i < receipt->discountCount; ++i) {
        printDiscount(buffer, &receipt->discounts[i]);
    }
    sprintf(buffer + strlen(buffer), "\n");

    print_total(buffer, receipt);

}

void printReceiptItem(char *buffer, struct receipt_item_t *item) {
    char price[MAX_NAME_LENGTH];
    sprintf(price, "%.2f", (*item).totalPrice);
    char name[MAX_NAME_LENGTH];
    sprintf(name, "%s", (*item).product->name);

    char line[LINE_LENGTH];
    print_line(line, name, price);
    sprintf(buffer + strlen(buffer), "%s", line);

    if (item->quantity != 1) {
        char line2[LINE_LENGTH];
        if (item->product->unit == Each) {
            sprintf(line2, "  %.2f*%.0f", item->price, item->quantity);
        } else {
            sprintf(line2, "  %.2f*%.3f", item->price, item->quantity);
        }
        sprintf(buffer + strlen(buffer), "%s\n", line2);
    }
}

void print_total(char *buffer, struct receipt_t *receipt) {
    char total[MAX_NAME_LENGTH];
    sprintf(total, "%0.2f", total_price(receipt));
    char total_line[LINE_LENGTH];
    print_line(total_line, "Total:", total);
    sprintf(buffer + strlen(buffer), "%s", total_line);
}

void printDiscount(char *buffer, struct discount_t *discount) {
    char name[LINE_LENGTH];
    sprintf(name, "%s(%s)", discount->description, discount->product->name);
    char price[MAX_NAME_LENGTH];
    sprintf(price, "%.2f", discount->amount);

    char line[LINE_LENGTH];
    print_line(line, name, price);
    sprintf(buffer + strlen(buffer), "%s", line);
}


void
print_line(char *buffer, const char *key, const char *value) {
    int whitespace_length = LINE_LENGTH - strlen(key) - strlen(value);
    char whitespace[whitespace_length];
    for (int i = 0; i < whitespace_length -1; ++i) {
        whitespace[i] = ' ';
    }
    whitespace[whitespace_length-1] = '\0';

    sprintf(buffer, "%s%s%s\n", key, whitespace, value );
}
