import {ProductUnit} from "./ProductUnit"

export class Product {

    constructor(private readonly name: string,
                private readonly unit: ProductUnit) {
    }

    public getName(): string {
        return this.name;
    }


    public getUnit(): ProductUnit {
        return this.unit;
    }



}
