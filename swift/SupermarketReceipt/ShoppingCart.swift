public class ShoppingCart {

    public var items = [ProductQuantity]()
    public var productQuantities = [Product: Double]()

    func addItem(product: Product) {
        self.addItemQuantity(product: product, quantity: 1.0)
    }

    public func addItemQuantity(product: Product , quantity: Double) {
        items.append(ProductQuantity(product: product, quantity:quantity))
        if productQuantities[product] != nil {
            productQuantities[product] = productQuantities[product]! + quantity
        } else {
            productQuantities[product] = quantity
        }
    }

    func handleOffers(receipt: Receipt, offers: [Product: Offer], catalog: SupermarketCatalog ) {
        for p in productQuantities.keys {
            var quantity = productQuantities[p]
            if offers[p] != nil {
                var offer = offers[p]
                var unitPrice = catalog.getUnitPrice(product: p)
                var quantityAsInt = Int(quantity!)
                var discount: Discount? = nil
                var x = 1
                if offer?.offerType == SpecialOfferType.ThreeForTwo {
                    x = 3

                } else if offer?.offerType == SpecialOfferType.TwoForAmount {
                    x = 2
                    if (quantityAsInt >= 2) {
                        
                        var intDivision = quantityAsInt / x
                        
                        var pricePerUnit = (offer!.argument * Double(intDivision))
                        
                        var theTotal = Double(quantityAsInt % 2) * unitPrice
                        
                        var total = pricePerUnit + theTotal
                        var discountN = unitPrice * quantity! - total
                        discount =  Discount(description:  "2 for \(offer!.argument)", discountAmount: discountN, product: p)
                    }

                } else if offer?.offerType == SpecialOfferType.FiveForAmount {
                    x = 5
                }
                var numberOfXs = quantityAsInt / x
                if offer?.offerType == SpecialOfferType.ThreeForTwo && quantityAsInt > 2 {
                    var left = Double(numberOfXs * 2) * unitPrice
                    var right = Double(quantityAsInt % 3) * unitPrice
                    var lastPart = left + right
                    var discountAmount = ((quantity ?? 1) * unitPrice) - lastPart
                    discount =  Discount(description: "3 for 2", discountAmount: discountAmount, product: p)
                }
                if offer?.offerType == SpecialOfferType.TenPercentDiscount {
                    discount =  Discount(description: "\(offer!.argument)% off", discountAmount: (quantity ?? 1) * unitPrice * (offer?.argument ?? 1) / 100.0, product: p)
                }
                if offer?.offerType == SpecialOfferType.FiveForAmount && quantityAsInt >= 5 {
                    var left = (unitPrice * (quantity ?? 1))
                    var right = ((offer?.argument ?? 1) * Double(numberOfXs)) + (Double(quantityAsInt % 5) * unitPrice)
                    var discountTotal = left - right
                    discount =  Discount(description: "\(x) for \(offer!.argument)", discountAmount: discountTotal,  product: p)
                }
                if discount != nil {
                    receipt.addDiscount(discount: discount!)
                }
            }

        }
    }
}
