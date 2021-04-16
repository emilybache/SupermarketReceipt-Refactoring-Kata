package supermarket

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestChecksOutArticlesFrom(t *testing.T) {
	// ARRANGE
	var toothbrush = Product{name: "toothbrush", unit: Each}
	var apples = Product{name: "apples", unit: Kilo}
	var catalog = NewFakeCatalog()
	catalog.addProduct(toothbrush, 0.99)
	catalog.addProduct(apples, 1.99)

	type products struct {
		product  Product
		quantity float64
	}
	cartItems := []struct {
		products       []products
		itemsCount     int
		discountsCount int
	}{
		{
			[]products{{toothbrush, 1.0}}, 1, 1,
		},
	}

	for _, item := range cartItems {
		var cart = NewShoppingCart()

		for _, cItem := range item.products {
			cart.addItemQuantity(cItem.product, cItem.quantity)
		}
		var teller = NewTeller(catalog)
		var offerProducts = make(map[Product]float64)
		offerProducts[toothbrush] = 1.0
		teller.addSpecialOffer(TenPercentDiscount, offerProducts, -0.09)
		var receipt = teller.checksOutArticlesFrom(cart)

		assert.Equal(t, item.itemsCount, len(receipt.items))
		assert.Equal(t, item.discountsCount, len(receipt.discounts))
	}
}
