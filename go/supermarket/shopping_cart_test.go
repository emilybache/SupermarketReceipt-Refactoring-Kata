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
		var offerProducts = make(map[Product]float64)
		offerProducts[toothbrush] = 1.0
		var cart = NewShoppingCart()
		var teller = NewTeller(catalog)
		teller.addSpecialOffer(item.offerType, offerProducts, item.argument)
		cart.addItemQuantity(toothbrush, item.quantity)
	}
}

func TestHandleOffersTenPercentDiscount(t *testing.T) {
	var toothbrush = Product{name: "toothbrush", unit: Each}
	var catalog = NewFakeCatalog()
	catalog.addProduct(toothbrush, 0.99)

	var cart = NewShoppingCart()
	var teller = NewTeller(catalog)
	var offerProducts = make(map[Product]float64)
	offerProducts[toothbrush] = 1.0
	teller.addSpecialOffer(TenPercentDiscount, offerProducts, -0.09)
	cart.addItemQuantity(toothbrush, 1.0)

	var receipt = teller.checksOutArticlesFrom(cart)
	assert.Equal(t, 1.0, float64(len(receipt.items)))
	assert.Equal(t, -0.09, receipt.discounts[0].discountAmount)
}

func TestHandleOffersThreeForTwo(t *testing.T) {
	var toothbrush = Product{name: "toothbrush", unit: Each}
	var catalog = NewFakeCatalog()
	catalog.addProduct(toothbrush, 0.99)

	var cart = NewShoppingCart()
	var teller = NewTeller(catalog)
	var offerProducts = make(map[Product]float64)
	offerProducts[toothbrush] = 3.0
	teller.addSpecialOffer(ThreeForTwo, offerProducts, -0.99)
	cart.addItemQuantity(toothbrush, 3.0)

	var receipt = teller.checksOutArticlesFrom(cart)
	assert.Equal(t, -0.99, receipt.discounts[0].discountAmount)
}

func TestHandleOffersTwoForAmount(t *testing.T) {
	var toothbrush = Product{name: "toothbrush", unit: Each}
	var catalog = NewFakeCatalog()
	catalog.addProduct(toothbrush, 0.99)

	var cart = NewShoppingCart()
	var teller = NewTeller(catalog)
	var offerProducts = make(map[Product]float64)
	offerProducts[toothbrush] = 2.0
	teller.addSpecialOffer(TwoForAmount, offerProducts, -0.48)
	cart.addItemQuantity(toothbrush, 2.0)

	var receipt = teller.checksOutArticlesFrom(cart)
	assert.Equal(t, -0.48, receipt.discounts[0].discountAmount)
}

func TestHandleOffersTwoForAmountMultiples(t *testing.T) {
	var toothbrush = Product{name: "toothbrush", unit: Each}
	var catalog = NewFakeCatalog()
	catalog.addProduct(toothbrush, 0.99)

	var cart = NewShoppingCart()
	var teller = NewTeller(catalog)
	var offerProducts = make(map[Product]float64)
	offerProducts[toothbrush] = 2.0
	teller.addSpecialOffer(TwoForAmount, offerProducts, -0.48)
	cart.addItemQuantity(toothbrush, 5.0)

	var receipt = teller.checksOutArticlesFrom(cart)
	assert.Equal(t, -0.96, receipt.discounts[0].discountAmount)
}

func TestHandleOffersFiveForAmount(t *testing.T) {
	var toothbrush = Product{name: "toothbrush", unit: Each}
	var catalog = NewFakeCatalog()
	catalog.addProduct(toothbrush, 0.99)

	var cart = NewShoppingCart()
	var teller = NewTeller(catalog)
	var offerProducts = make(map[Product]float64)
	offerProducts[toothbrush] = 5.0
	teller.addSpecialOffer(FiveForAmount, offerProducts, -0.85)
	cart.addItemQuantity(toothbrush, 5.0)

	var receipt = teller.checksOutArticlesFrom(cart)
	assert.Equal(t, -0.85, receipt.discounts[0].discountAmount)
}

func TestHandleOffersBundle(t *testing.T) {
	var toothbrush = Product{name: "toothbrush", unit: Each}
	var mouthwash = Product{name: "mouthwash", unit: Each}
	var dentalfloss = Product{name: "dentalfloss", unit: Each}
	var catalog = NewFakeCatalog()
	catalog.addProduct(toothbrush, 0.99)
	catalog.addProduct(mouthwash, 1.11)
	catalog.addProduct(dentalfloss, 1.00)

	var cart = NewShoppingCart()
	var teller = NewTeller(catalog)
	var offerProducts = make(map[Product]float64)
	offerProducts[toothbrush] = 1.0
	offerProducts[mouthwash] = 1.0
	offerProducts[dentalfloss] = 1.0
	teller.addSpecialOffer(TwoForAmount, offerProducts, -0.60)
	cart.addItemQuantity(toothbrush, 1.0)
	cart.addItemQuantity(mouthwash, 1.0)
	cart.addItemQuantity(dentalfloss, 1.0)

	var receipt = teller.checksOutArticlesFrom(cart)
	assert.Equal(t, -0.60, receipt.discounts[0].discountAmount)
	assert.Equal(t, 1, len(receipt.discounts))
	// TODO: refactor receipt.totalPrice to return a rounded float64
	assert.Equal(t, 2.50, math.Floor(receipt.totalPrice()*100)/100)
}

func TestHandleOffersBundleWithExtras(t *testing.T) {
	var toothbrush = Product{name: "toothbrush", unit: Each}
	var mouthwash = Product{name: "mouthwash", unit: Each}
	var dentalfloss = Product{name: "dentalfloss", unit: Each}
	var catalog = NewFakeCatalog()
	catalog.addProduct(toothbrush, 0.99)
	catalog.addProduct(mouthwash, 1.11)
	catalog.addProduct(dentalfloss, 1.00)

	var cart = NewShoppingCart()
	var teller = NewTeller(catalog)
	var offerProducts = make(map[Product]float64)
	offerProducts[toothbrush] = 1.0
	offerProducts[mouthwash] = 1.0
	offerProducts[dentalfloss] = 1.0
	teller.addSpecialOffer(TwoForAmount, offerProducts, -0.60)
	cart.addItemQuantity(toothbrush, 1.0)
	cart.addItemQuantity(toothbrush, 1.0)
	cart.addItemQuantity(mouthwash, 1.0)
	cart.addItemQuantity(mouthwash, 1.0)
	cart.addItemQuantity(dentalfloss, 1.0)

	var receipt = teller.checksOutArticlesFrom(cart)
	assert.Equal(t, -0.60, receipt.discounts[0].discountAmount)
	assert.Equal(t, 1, len(receipt.discounts))
	// TODO: refactor receipt.totalPrice to return a rounded float64
	assert.Equal(t, 4.60, math.Floor(receipt.totalPrice()*100)/100)
}

func TestHandleOffersIncompleteBundle(t *testing.T) {
	var toothbrush = Product{name: "toothbrush", unit: Each}
	var mouthwash = Product{name: "mouthwash", unit: Each}
	var dentalfloss = Product{name: "dentalfloss", unit: Each}
	var catalog = NewFakeCatalog()
	catalog.addProduct(toothbrush, 0.99)
	catalog.addProduct(mouthwash, 1.11)
	catalog.addProduct(dentalfloss, 1.00)

	var cart = NewShoppingCart()
	var teller = NewTeller(catalog)
	var offerProducts = make(map[Product]float64)
	offerProducts[toothbrush] = 1.0
	offerProducts[mouthwash] = 1.0
	offerProducts[dentalfloss] = 1.0
	teller.addSpecialOffer(TwoForAmount, offerProducts, -0.60)
	cart.addItemQuantity(toothbrush, 1.0)
	cart.addItemQuantity(mouthwash, 1.0)

	var receipt = teller.checksOutArticlesFrom(cart)
	assert.Equal(t, 0, len(receipt.discounts))
	assert.Equal(t, 2.10, receipt.totalPrice())
}
