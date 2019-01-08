import {Product} from "./Product"
import {SpecialOfferType} from "./SpecialOfferType"

export class Offer {

    public constructor(public offerType: SpecialOfferType,
                       private readonly product: Product ,
                       public argument: number) {
    }

    getProduct(): Product {
        return this.product;
    }

}
