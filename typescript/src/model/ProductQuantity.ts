import {Product} from "./Product"

export class ProductQuantity {
    private readonly product: Product;
    private readonly quantity: number;

    constructor(product: Product,
                weight: number) {
        this.product = product;
        this.quantity = weight;
    }

    public getProduct(): Product {
        return this.product;
    }

    public getQuantity(): number {
        return this.quantity;
    }

}
