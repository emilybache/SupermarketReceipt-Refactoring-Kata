//
// Created by sdargo on 2/4/19.
//

#ifndef CPP_RECEIPTPRINTER_H
#define CPP_RECEIPTPRINTER_H


#include "model/Receipt.h"

#include <iomanip>
#include <sstream>

class ReceiptPrinter {

public:
    ReceiptPrinter() : ReceiptPrinter(40) {
        }

    ReceiptPrinter(int columns) : columns(columns) {}

    std::string printReceipt(const Receipt& receipt) {
        std::string result;
            for (const auto& item : receipt.getItems()) {
                std::string price = getFormattedNumberAsString(item.getTotalPrice(), 2);
                std::string quantity = presentQuantity(item);
                std::string name = item.getProduct().getName();
                std::string unitPrice = getFormattedNumberAsString(item.getPrice(), 2);

                int whitespaceSize = columns - name.length() - price.length();
                std::string line = name + getWhitespace(whitespaceSize) + price + "\n";

                if (item.getQuantity() != 1) {
                    line += "  " + unitPrice + " * " + quantity + "\n";
                }
                result.append(line);
            }
            for (const auto& discount : receipt.getDiscounts()) {
                std::string productPresentation = discount.getProduct().getName();
                std::string pricePresentation = getFormattedNumberAsString(discount.getDiscountAmount(), 2);
                std::string description = discount.getDescription();
                result.append(description);
                result.append("(");
                result.append(productPresentation);
                result.append(")");
                result.append(getWhitespace(columns - 3 - productPresentation.length() - description.length() - pricePresentation.length()));
                result.append("-");
                result.append(pricePresentation);
                result.append("\n");
            }
            result.append("\n");
            std::string pricePresentation = getFormattedNumberAsString(receipt.getTotalPrice(), 2);
            std::string total = "Total: ";
            std::string whitespace = getWhitespace(columns - total.length() - pricePresentation.length());
            result.append(total).append(whitespace).append(pricePresentation);
            return result;
        }

    static std::string presentQuantity(const ReceiptItem& item) {
            return ProductUnit::Each == item.getProduct().getUnit()
                   ? getFormattedNumberAsString(item.getQuantity(), 0)
                   : getFormattedNumberAsString(item.getQuantity(), 3);
        }

    static std::string getWhitespace(int whitespaceSize) {
            std::string whitespace;
            for (int i = 0; i < whitespaceSize; i++) {
                whitespace.append(" ");
            }
            return whitespace;
        }
private:

    static std::string getFormattedNumberAsString(double number, int precision) {
        std::stringstream stream;
        stream << std::fixed << std::setprecision(precision) << number;
        return stream.str();
    }
    
    const int columns;


};


#endif //CPP_RECEIPTPRINTER_H
