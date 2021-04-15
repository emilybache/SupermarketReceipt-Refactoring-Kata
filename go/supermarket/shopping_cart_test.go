package supermarket

import (
	"math"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestAddItem(t *testing.T) {
	var toothbrush = Product{name: "toothbrush", unit: Each}
	var apples = Product{name: "apples", unit: Kilo}
	var catalog = NewFakeCatalog()
	catalog.addProduct(toothbrush, 0.99)
	catalog.addProduct(apples, 1.99)

	items := []struct {
		product  Product
		quantity float64
	}{
		{toothbrush, 1.0},
		{toothbrush, 2.0},
		{toothbrush, 3.0},
	}

	for _, item := range items {
		var cart = NewShoppingCart()

		for i := 0; i < int(item.quantity); i++ {
			cart.addItem(item.product)
		}

		assert.Equal(t, item.quantity, cart.productQuantities[item.product])
	}
}

func TestAddItemQuantity(t *testing.T) {
	var toothbrush = Product{name: "toothbrush", unit: Each}
	var apples = Product{name: "apples", unit: Kilo}
	var catalog = NewFakeCatalog()
	catalog.addProduct(toothbrush, 0.99)
	catalog.addProduct(apples, 1.99)

	items := []struct {
		product  Product
		quantity float64
	}{
		{apples, 1.0},
		{apples, 2.3},
		{apples, 38.9},
	}

	for _, item := range items {
		var cart = NewShoppingCart()
		cart.addItemQuantity(item.product, item.quantity)
		assert.Equal(t, item.quantity, cart.productQuantities[item.product])
	}
}

func TestHandleOffers(t *testing.T) {
	var toothbrush = Product{name: "toothbrush", unit: Each}
	var catalog = NewFakeCatalog()
	catalog.addProduct(toothbrush, 0.99)

	items := []struct {
		quantity  float64
		offerType SpecialOfferType
		argument  float64
	}{
		{1.0, TenPercentDiscount, 10.0},
		{3.0, ThreeForTwo, 1.98},
		{2.0, TwoForAmount, 1.60},
		{5.0, FiveForAmount, 4.10},
	}

	for _, item := range items {
		var cart = NewShoppingCart()
		var teller = NewTeller(catalog)
		teller.addSpecialOffer(item.offerType, toothbrush, item.argument)
		cart.addItemQuantity(toothbrush, item.quantity)

		// TODO: refactor special offers to make this easier to test
		// var receipt = teller.checksOutArticlesFrom(cart)
		// assert.Equal(t, item.quantity, receipt.items)
	}
}

func TestHandleOffersTenPercentDiscount(t *testing.T) {
	var toothbrush = Product{name: "toothbrush", unit: Each}
	var catalog = NewFakeCatalog()
	catalog.addProduct(toothbrush, 0.99)

	var cart = NewShoppingCart()
	var teller = NewTeller(catalog)
	teller.addSpecialOffer(TenPercentDiscount, toothbrush, 10.0)
	cart.addItemQuantity(toothbrush, 1.0)

	var receipt = teller.checksOutArticlesFrom(cart)
	assert.Equal(t, 1.0, float64(len(receipt.items)))
	assert.Equal(t, -0.1, math.Round(receipt.discounts[0].discountAmount*100)/100)
}

func TestHandleOffersThreeForTwo(t *testing.T) {
	var toothbrush = Product{name: "toothbrush", unit: Each}
	var catalog = NewFakeCatalog()
	catalog.addProduct(toothbrush, 0.99)

	var cart = NewShoppingCart()
	var teller = NewTeller(catalog)
	teller.addSpecialOffer(ThreeForTwo, toothbrush, -0.99)
	cart.addItemQuantity(toothbrush, 3.0)

	var receipt = teller.checksOutArticlesFrom(cart)
	assert.Equal(t, -0.99, math.Round(receipt.discounts[0].discountAmount*100)/100)
}

func TestHandleOffersTwoForAmount(t *testing.T) {
	var toothbrush = Product{name: "toothbrush", unit: Each}
	var catalog = NewFakeCatalog()
	catalog.addProduct(toothbrush, 0.99)

	var cart = NewShoppingCart()
	var teller = NewTeller(catalog)
	teller.addSpecialOffer(TwoForAmount, toothbrush, 1.50)
	cart.addItemQuantity(toothbrush, 2.0)

	var receipt = teller.checksOutArticlesFrom(cart)
	assert.Equal(t, -0.48, math.Round(receipt.discounts[0].discountAmount*100)/100)
}

func TestHandleOffersFiveForAmount(t *testing.T) {
	var toothbrush = Product{name: "toothbrush", unit: Each}
	var catalog = NewFakeCatalog()
	catalog.addProduct(toothbrush, 0.99)

	var cart = NewShoppingCart()
	var teller = NewTeller(catalog)
	teller.addSpecialOffer(FiveForAmount, toothbrush, 4.10)
	cart.addItemQuantity(toothbrush, 5.0)

	var receipt = teller.checksOutArticlesFrom(cart)
	assert.Equal(t, -0.85, math.Round(receipt.discounts[0].discountAmount*100)/100)
}
