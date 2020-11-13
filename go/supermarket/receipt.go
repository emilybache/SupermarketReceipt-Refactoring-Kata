package supermarket

import "sort"

type Receipt struct {
    items []ReceiptItem
    discounts []Discount
}

type ReceiptItem struct {
    product Product
    quantity float64
    price float64
    totalPrice float64
}

func NewReceipt() *Receipt {
    var r Receipt
    r.items = []ReceiptItem{}
    r.discounts = []Discount{}
    return &r
}

func (r *Receipt) totalPrice() float64 {
    var total float64 = 0
    for _, item := range r.items {
        total += item.totalPrice
    }
    for _, discount := range r.discounts {
        total += discount.discountAmount
    }
    return total
}


func (r *Receipt) addProduct(product Product, quantity float64, unitPrice float64, price float64) {
    r.items = append(r.items, ReceiptItem{
        product:    product,
        quantity:   quantity,
        price:      unitPrice,
        totalPrice: price,
    })
}

func (r *Receipt) addDiscount(discount Discount) {
    r.discounts = append(r.discounts, discount)
}

func (r Receipt) sortedItems() []ReceiptItem {
    sort.Slice(r.items, func(i, j int) bool {
        return r.items[i].product.name < r.items[j].product.name
    })
    return r.items
}

func (r Receipt) sortedDiscounts() []Discount {
    sort.Slice(r.discounts, func(i, j int) bool {
        return r.discounts[i].product.name < r.discounts[j].product.name
    })
    return r.discounts
}
