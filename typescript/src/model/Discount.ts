import {Product} from "./Product"

export class Discount {

    constructor(private readonly product: Product,
                private readonly description: string,
                private readonly discountAmount: number) {
    }

    getDescription(): string {
        return this.description;
    }

    getDiscountAmount(): number {
        return this.discountAmount;
    }

    getProduct(): Product {
        return this.product;
    }

}
