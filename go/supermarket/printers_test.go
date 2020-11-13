package supermarket
import (
	approvals "github.com/approvals/go-approval-tests"
	"github.com/approvals/go-approval-tests/reporters"
	"os"
	"testing"
)

func TestMain(m *testing.M) {
	r := approvals.UseReporter(reporters.NewContinuousIntegrationReporter())
	defer r.Close()

	os.Exit(m.Run())
}

type LineItem struct {
	product Product
	quantity float64
	unitPrice float64
	totalPrice float64
}

func TestReceiptPrinter(t *testing.T) {
	var toothbrush = Product{name: "toothbrush", unit: Each}
	var apples = Product{name: "apples", unit: Kilo}
	var printer = NewReceiptPrinter()

	// Can't run in parallel while all tests write to same string
	//t.Parallel()
	tests := []struct {
		name  string
		products []LineItem
		discounts []Discount
	}{
		{
			name: "one line item",
			products: []LineItem{{product: toothbrush, quantity: 1, unitPrice: 0.99, totalPrice: 0.99}},
			discounts: []Discount{},
		},
		{
			name: "two line items",
			products: []LineItem{{product: toothbrush, quantity: 2, unitPrice: 0.99, totalPrice: 0.99*2}},
			discounts: []Discount{},
		},
		{
			name: "looseWeight",
			products: []LineItem{{product: apples, quantity: 2.3, unitPrice: 1.99, totalPrice: 1.99*2.3}},
			discounts: []Discount{},
		},
		{
			name: "total",
			products: []LineItem{{product: toothbrush, quantity: 1, unitPrice: 0.99, totalPrice: 0.99},
				{product: apples, quantity: 0.75, unitPrice: 1.99, totalPrice: 1.99*0.75}},
			discounts: []Discount{},
		},
		{
			name: "discounts",
			products: []LineItem{{product: apples, quantity: 2.3, unitPrice: 1.99, totalPrice: 1.99*2.3}},
			discounts: []Discount{{
				product:        apples,
				description:    "3 for 2",
				discountAmount: -0.99,
			}},
		},
		{
			name: "wholeReceipt",
			products: []LineItem{{product: toothbrush, quantity: 1, unitPrice: 0.99, totalPrice: 0.99},
				{product: toothbrush, quantity: 2, unitPrice: 0.99, totalPrice: 0.99*2},
				{product: apples, quantity: 0.75, unitPrice: 1.99, totalPrice: 1.99*0.75},
			},
			discounts: []Discount{{
				product:        toothbrush,
				description:    "3 for 2",
				discountAmount: -0.99,
			}},
		},
	}
	var toVerify string
	for _, tc := range tests {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			var receipt = NewReceipt()
			for _, pq := range tc.products {
				receipt.addProduct(pq.product, pq.quantity, pq.unitPrice, pq.totalPrice)

			}
			for _, d := range tc.discounts {
				receipt.addDiscount(d)
			}
			toVerify += "\n========================================\n\n"
			toVerify += printer.printReceipt(receipt)
		})
	}
	// TODO: make these into separate data driven tests when approvals supports that
	approvals.VerifyString(t, toVerify)

}
