package supermarket


type Teller struct {
    catalog Catalog
    offers map[Product]SpecialOffer
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

func (t *Teller) addSpecialOffer(offerType SpecialOfferType, product Product, argument float64) {
	var offer = SpecialOffer{offerType: offerType, product: product, argument: argument}
	t.offers[product] = offer
}
