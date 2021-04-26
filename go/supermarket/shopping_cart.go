package supermarket

import "math"

type ProductQuantity struct {
	product  Product
	quantity float64
}

type ShoppingCart struct {
	items             []ProductQuantity
	productQuantities map[Product]float64
}

func NewShoppingCart() *ShoppingCart {
	var s ShoppingCart
	s.items = []ProductQuantity{}
	s.productQuantities = make(map[Product]float64)
	return &s
}

func (c *ShoppingCart) addItem(product Product) {
	c.addItemQuantity(product, 1)
}

func (c *ShoppingCart) addItemQuantity(product Product, amount float64) {
	c.items = append(c.items, ProductQuantity{product: product, quantity: amount})
	currentAmount, ok := c.productQuantities[product]
	if ok {
		c.productQuantities[product] = currentAmount + amount
	} else {
		c.productQuantities[product] = amount
	}
}

// TODO: move this to the teller, it's similar to checking out
//   and shouldn't be handled by the cart for correctness/security
func (c *ShoppingCart) handleOffers(receipt *Receipt, offers map[Product]SpecialOffer, catalog Catalog) {
CartItems:
	for p, quantity := range c.productQuantities {
		// check if there is an offer for this product
		if offer, ok := offers[p]; ok {

			// check all the products in an offer, in case it's a bundle
			for offerProduct, offerQuantity := range offers[p].products {

				// check the val and the key, since we might be checking for
				//   a bundle item that isn't in the cart, hence wasn't purchased
				val, ok := c.productQuantities[offerProduct]
				if !ok || val < offerQuantity {
					continue CartItems
				}
			}

			var multiplier float64

			// check if there is more than one product for this offer, hence
			//   it must be a bundle, thus the multiplier should only be 1.0
			if len(offers[p].products) > 1 {
				multiplier = 1.0
			} else {
				multiplier = math.Floor(quantity / offers[p].products[p])
			}

			receipt.addDiscount(Discount{product: p, description: offer.description, discountAmount: offer.discount * multiplier})

			// Make sure we don't apply the same offer multiple times
			//   for different items in the same bundle
			for bundleItem := range offers[p].products {
				delete(offers, bundleItem)
			}
		}
	}
}
