import {Product} from "./Product"
import {SpecialOfferType} from "./SpecialOfferType"

export class Offer {

    public constructor(public readonly offerType: SpecialOfferType,
                       public readonly product: Product,
                       public readonly argument: number) {
    }

    getProduct(): Product {
        return this.product;
    }

}
