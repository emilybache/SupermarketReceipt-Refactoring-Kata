

#ifndef SUPERMARKETRECEIPT_PRINTER_H
#define SUPERMARKETRECEIPT_PRINTER_H

#include "supermarket.h"

// maximum buffer size
#define MAX_PRINT_LENGTH 20000
// lines are fixed width
#define LINE_LENGTH 60

void print_receipt(char* buffer, struct receipt_t* receipt);
void print_line(char *buffer, const char *key, const char *value);
void print_total(char *buffer, struct receipt_t *receipt);

#endif //SUPERMARKETRECEIPT_PRINTER_H
