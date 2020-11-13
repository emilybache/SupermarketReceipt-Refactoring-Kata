package supermarket

import (
	"fmt"
	"golang.org/x/text/language"
	"golang.org/x/text/message"
	"strings"
)

type ReceiptPrinter struct {
	columns int
	lp *message.Printer
}

func NewReceiptPrinter() *ReceiptPrinter {
	var p ReceiptPrinter
	p.columns = 40
	p.lp = message.NewPrinter(language.BritishEnglish)
	return &p
}

func (p ReceiptPrinter) printReceipt(receipt *Receipt) string {
	var result string
	for _, item := range receipt.sortedItems() {
		var receiptItem = p.presentReceiptItem(item)
		result += receiptItem
	}
	for _, discount := range receipt.sortedDiscounts() {
		result += p.presentDiscount(discount)
	}
	result += "\n"
	result += p.presentTotal(receipt)

	return result
}

func (p ReceiptPrinter) presentReceiptItem(item ReceiptItem) string {
	var totalPricePresentation string = p.presentPrice(item.totalPrice)
	var line = p.formatLineWithWhitespace(item.product.name, totalPricePresentation)
	if item.quantity != 1 {
		line += fmt.Sprintf("  %s * %s\n", p.presentPrice(item.price), p.presentQuantity(item))
	}
	return line
}

func (p ReceiptPrinter) formatLineWithWhitespace(name string, value string) string {
	var result strings.Builder
	fmt.Fprint(&result, name)
	var whitespaceSize = p.columns - len(name) - len(value)
	for i := 1; i <= whitespaceSize; i++ {
		fmt.Fprint(&result, " ")
	}
	fmt.Fprintf(&result, "%s\n", value)
	return result.String()
}

func (p ReceiptPrinter) presentPrice(price float64) string {
	return p.lp.Sprintf("%.2f", price)
}

func (p ReceiptPrinter) presentQuantity(item ReceiptItem) string {
	var result string
	if Each == item.product.unit {
		result = fmt.Sprintf("%d", int(item.quantity))
	} else {
		result = p.lp.Sprintf("%.3f", item.quantity)
	}
	return result
}

func (p ReceiptPrinter) presentTotal(receipt *Receipt) string {
	var name = "Total: "
	var value = p.presentPrice(receipt.totalPrice())
	return p.formatLineWithWhitespace(name, value)
}

func (p ReceiptPrinter) presentDiscount(discount Discount) string {
	var name = fmt.Sprintf("%s (%s)", discount.description, discount.product.name)
	var value = p.presentPrice(discount.discountAmount)
	return p.formatLineWithWhitespace(name, value)
}

