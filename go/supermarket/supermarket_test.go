package supermarket

import (
	"github.com/approvals/go-approval-tests"
	"testing"
)

type FakeCatalog struct {
	_products map[string]Product
	_prices   map[string]float64
}

func (c FakeCatalog) unitPrice(product Product) float64 {
	return c._prices[product.name]
}

func (c FakeCatalog) addProduct(product Product, price float64) {
	c._products[product.name] = product
	c._prices[product.name] = price
}

func NewFakeCatalog() *FakeCatalog {
	var c FakeCatalog
	c._products = make(map[string]Product)
	c._prices = make(map[string]float64)
	return &c
}

type Offer struct {
	offerType SpecialOfferType
	product   Product
	argument  float64
}

func TestSupermarket(t *testing.T) {
	var toothbrush = Product{name: "toothbrush", unit: Each}
	var apples = Product{name: "apples", unit: Kilo}
	var rice = Product{name: "rice", unit: Each}
	var cherryTomatoes = Product{name: "cherry tomato box", unit: Each}
	var catalog = NewFakeCatalog()
	catalog.addProduct(toothbrush, 0.99)
	catalog.addProduct(apples, 1.99)
	catalog.addProduct(rice, 2.99)
	catalog.addProduct(cherryTomatoes, 0.69)

	var teller = NewTeller(catalog)
	var printer = NewReceiptPrinter()

	tests := []struct {
		name       string
		products   []Product
		offers     []Offer
		quantities []float64
	}{
		{
			name:     "empty shoppingcart",
			products: []Product{},
			offers:   []Offer{},
		},
		{
			name:     "one normal item",
			products: []Product{toothbrush},
			offers:   []Offer{},
		},
		{
			name:     "two normal items",
			products: []Product{toothbrush, rice},
			offers:   []Offer{},
		},
		{
			name:     "buy two get one free",
			products: []Product{toothbrush, toothbrush, toothbrush},
			offers:   []Offer{{ThreeForTwo, toothbrush, catalog.unitPrice(toothbrush)}},
		},
		{
			name:     "buy two get one free but insufficient",
			products: []Product{toothbrush},
			offers:   []Offer{{ThreeForTwo, toothbrush, catalog.unitPrice(toothbrush)}},
		},
		{
			name:     "buy five get one free",
			products: []Product{toothbrush, toothbrush, toothbrush, toothbrush, toothbrush},
			offers:   []Offer{{ThreeForTwo, toothbrush, catalog.unitPrice(toothbrush)}},
		},
		{
			name:       "loose weight product",
			products:   []Product{apples},
			quantities: []float64{0.5},
			offers:     []Offer{},
		},
		{
			name:     "percent discount",
			products: []Product{rice},
			offers:   []Offer{{TenPercentDiscount, rice, 10.0}},
		},
		{
			name:     "XForY discount",
			products: []Product{cherryTomatoes, cherryTomatoes},
			offers:   []Offer{{TwoForAmount, cherryTomatoes, 0.99}},
		},
		{
			name:     "XForY discount with insufficient in basket",
			products: []Product{cherryTomatoes},
			offers:   []Offer{{TwoForAmount, cherryTomatoes, 0.99}},
		},
		{
			name:       "FiveForY discount",
			products:   []Product{apples},
			quantities: []float64{5},
			offers:     []Offer{{FiveForAmount, apples, 6.99}},
		},
		{
			name:       "FiveForY discount with six",
			products:   []Product{apples},
			quantities: []float64{6},
			offers:     []Offer{{FiveForAmount, apples, 6.99}},
		},
		{
			name:       "FiveForY discount with sixteen",
			products:   []Product{apples},
			quantities: []float64{16},
			offers:     []Offer{{FiveForAmount, apples, 6.99}},
		},
		{
			name:       "FiveForY discount with four",
			products:   []Product{apples},
			quantities: []float64{4},
			offers:     []Offer{{FiveForAmount, apples, 6.99}},
		},
	}
	t.Parallel()
	for _, tc := range tests {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			var cart = NewShoppingCart()
			for i, product := range tc.products {
				if len(tc.quantities)-1 >= i {
					cart.addItemQuantity(product, tc.quantities[i])
				} else {
					cart.addItem(product)
				}
			}
			for _, offer := range tc.offers {
				teller.addSpecialOffer(offer.offerType, offer.product, offer.argument)
			}
			var receipt = teller.checksOutArticlesFrom(cart)
			approvals.VerifyString(t, printer.printReceipt(receipt))
		})
	}

}
