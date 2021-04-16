package supermarket

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

func (c *ShoppingCart) handleOffers(receipt *Receipt, offers map[Product]SpecialOffer, catalog Catalog) {
	for p, quantity := range c.productQuantities {
		if offer, ok := offers[p]; ok {

			if offers[p].products[p] < quantity {
				continue
			}

			receipt.addDiscount(Discount{product: p, description: offer.description, discountAmount: offer.discount})
		}

	}
}
