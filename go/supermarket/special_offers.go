package supermarket

type SpecialOfferType int

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
