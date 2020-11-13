package supermarket

type SpecialOfferType int

const (
	TenPercentDiscount SpecialOfferType = iota
	ThreeForTwo
	TwoForAmount
	FiveForAmount
)

type SpecialOffer struct {
	offerType SpecialOfferType
	product Product
	argument float64
}

type Discount struct {
	product Product
	description string
	discountAmount float64
}

