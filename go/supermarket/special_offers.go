package supermarket

type SpecialOfferType int

// TODO: remove these, they aren't needed anymore
const (
	TenPercentDiscount SpecialOfferType = iota
	ThreeForTwo
	TwoForAmount
	FiveForAmount
)

type SpecialOffer struct {
	offerType   SpecialOfferType
	products    map[Product]float64
	description string
	discount    float64
}

type Discount struct {
	product        Product
	description    string
	discountAmount float64
}
