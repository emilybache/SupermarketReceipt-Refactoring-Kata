import {ProductUnit} from "./ProductUnit"

export class Product {

    constructor(public readonly name: string,
                public readonly unit: ProductUnit) {
    }
}
