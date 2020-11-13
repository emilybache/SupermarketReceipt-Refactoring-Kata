package supermarket

import (
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"testing"
)

type FakeCatalog struct {
    _products map[string]Product
    _prices map[string]float64
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

func TestTenPercentDiscount(t *testing.T) {
	// ARRANGE
	var toothbrush = Product{name: "toothbrush", unit: Each}
	var apples = Product{name: "apples", unit: Kilo}
	var catalog = NewFakeCatalog()
	catalog.addProduct(toothbrush, 0.99)
	catalog.addProduct(apples, 1.99)

	var teller = NewTeller(catalog)
	teller.addSpecialOffer(TenPercentDiscount, toothbrush, 10.0)

	var cart = NewShoppingCart()
	cart.addItemQuantity(apples, 2.5)

	// ACT
	var receipt = teller.checksOutArticlesFrom(cart)

	// ASSERT
	assert.Equal(t, 4.975, receipt.totalPrice())
	assert.Equal(t, 0, len(receipt.discounts))
	require.Equal(t, 1, len(receipt.items))
	var receiptItem = receipt.items[0]
    assert.Equal(t, 1.99, receiptItem.price)
	assert.Equal(t, 2.5*1.99, receiptItem.totalPrice)
	assert.Equal(t, 2.5, receiptItem.quantity)
}
