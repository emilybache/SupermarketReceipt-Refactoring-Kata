package supermarket

type Teller struct {
	catalog Catalog
	offers  map[Product]SpecialOffer
}

func NewTeller(catalog Catalog) *Teller {
	var t Teller
	t.catalog = catalog
	t.offers = make(map[Product]SpecialOffer)
	return &t
}

func (t *Teller) checksOutArticlesFrom(cart *ShoppingCart) *Receipt {
	var receipt = NewReceipt()
	for p, q := range cart.productQuantities {
		var unitPrice = t.catalog.unitPrice(p)
		var price = q * unitPrice
		receipt.addProduct(p, q, unitPrice, price)
	}
	cart.handleOffers(receipt, t.offers, t.catalog)
	return receipt
}

func (t *Teller) addSpecialOffer(offerType SpecialOfferType, products map[Product]float64, discount float64) {
	var offer = SpecialOffer{offerType: offerType, products: products, discount: discount}
	for product := range products {
		// TODO: update this to be a slice or similar, currently this
		//   means that a product can only ever have a single offer
		t.offers[product] = offer
	}
}
