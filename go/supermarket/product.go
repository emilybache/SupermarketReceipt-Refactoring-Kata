package supermarket

type ProductUnit int

type Catalog interface {
	unitPrice(product Product) float64
}

const (
	Each ProductUnit = iota
	Kilo
)

type Product struct {
	name string
	unit ProductUnit
}
